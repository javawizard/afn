
from threading import RLock, Condition
import autobus2
from autobus2 import exceptions, common
from afn.utils import wrap, print_exceptions
from afn.utils.concurrent import synchronized_on
from functools import partial

class SingleServiceProxy(common.AutoClose):
    def __init__(self, bus, info_filter):
        self.bus = bus
        self.info_filter = info_filter
        self.lock = RLock()
        self.bind_condition = Condition(self.lock)
        self.service_map = {}
        self.connection = None
        self.is_alive = True
        self.service_event_wrapper = wrap(self.service_event)
        bus.add_service_listener(self.service_event_wrapper, info_filter, True)
    
    @synchronized_on("lock")
    def service_event(self, service_id, host, port, info, event):
        if not self.is_alive:
            return
        getattr(self, "service_" + event.short_name.lower())(service_id, host, port, info)
    
    def service_discovered(self, service_id, host, port, info):
        self.service_map[service_id] = (host, port)
        if self.connection is None:
            self.connect_to(service_id)
    
    def service_changed(self, service_id, host, port, info):
        self.service_map[service_id] = (host, port)
        if self.connection is not None and self.connection.service_id == service_id:
            self.connect_to(service_id)
    
    def service_undiscovered(self, service_id, host, port, info):
        del self.service_map[service_id]
        if self.connection is not None and self.connection.service_id == service_id:
            self.connect_to(None)
    
    def connect_to(self, service_id):
        if self.connection is not None:
            self.connection.close()
            self.connection = None
        if service_id:
            host, port = self.service_map[service_id]
            self.connection = self.bus.connect(host, port, service_id, open_listener=self.connection_opened,
                                               close_listener=self.connection_closed, lock=self.lock)
    
    def connection_opened(self, connection):
        with self.lock:
            self.bind_condition.notify_all()
    
    def connection_closed(self, connection):
        pass
    
    def __getitem__(self, name):
        return SingleServiceFunction(self, name)
    
    @synchronized_on("lock")
    def close(self):
        if not self.is_alive: # Already closed
            return
        self.is_alive = False
        self.connect_to(None)
        self.bus.remove_service_listener(self.service_event_wrapper)
    
    def wait_for_bind(self, timeout=10):
        with self.lock:
            if self.connection:
                if self.connection.is_connected:
                    return
            self.bind_condition.wait(timeout)
            if self.connection:
                if self.connection.is_connected:
                    return
            raise exceptions.TimeoutException()
    
    @property
    @synchronized_on("lock")
    def current_service_id(self):
        if self.connection:
            return self.connection.service_id
        return None


class SingleServiceFunction(object):
    def __init__(self, proxy, name):
        self.proxy = proxy
        self.name = name
    
    def __call__(self, *args, **kwargs):
        with self.proxy.lock:
            if self.proxy.connection is None:
                raise exceptions.NotConnectedException
            function = self.proxy.connection[self.name]
        return function(*args, **kwargs)

