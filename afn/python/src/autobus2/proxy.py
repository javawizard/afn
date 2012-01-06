
from threading import RLock
import autobus2
from autobus2 import exceptions
from afn.utils import wrap
from afn.utils.concurrent import synchronized_on

class SingleServiceProxy(object):
    def __init__(self, bus, info_filter):
        self.bus = bus
        self.info_filter = info_filter
        self.lock = RLock()
        self.service_map = {}
        self.connection = None
        self.is_alive = True
        self.service_event_wrapper = wrap(self.service_event)
        bus.add_service_listener(self.service_event_wrapper, info_filter, True)
    
    @synchronized_on("lock")
    def service_event(self, service_id, host, port, info, event):
        getattr(self, "service_" + event.short_name.lower())(service_id, host, port, info)
    
    def service_discovered(self, service_id, host, port, info):
        self.service_map[service_id] = (host, port)
        if self.connection is None:
            self.connection = self.connect_to(service_id)
    
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
            self.connection = self.bus.connect(host, port, service_id, lock=self.lock)
    
    def __getitem__(self, name):
        return SingleServiceFunction(self, name)
    
    @synchronized_on("lock")
    def close(self):
        if not self.is_alive: # Already closed
            return
        self.is_alive = False
        self.connect_to(None)
        self.bus.remove_service_listener(self.service_event_wrapper)


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
















































