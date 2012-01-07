
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


class MultipleServiceProxy(common.AutoClose):
    def __init__(self, bus, info_filter, bind_function=None, unbind_function=None):
        self.bus = bus
        self.info_filter = info_filter
        self.lock = RLock()
        self.bind_condition = Condition(self.lock)
        self.bind_function = bind_function
        self.unbind_function = unbind_function
        self.service_map = {} # Map of service ids to connections
        self.is_alive = True
        self.service_event_wrapper = wrap(self.service_event)
        bus.add_service_listener(self.service_event_wrapper, info_filter, True)
    
    @synchronized_on("lock")
    def service_event(self, service_id, host, port, info, event):
        if not self.is_alive:
            return
        getattr(self, "service_" + event.short_name.lower())(service_id, host, port, info)
    
    def service_discovered(self, service_id, host, port, info):
        self.connect_to(service_id, host, port, info)
    
    def service_changed(self, service_id, host, port, info):
        self.disconnect_from(service_id)
        self.connect_to(service_id, host, port, info)
    
    def service_undiscovered(self, service_id, host, port, info):
        self.disconnect_from(service_id)
    
    def connect_to(self, service_id, host, port, info):
        self.service_map[service_id] = self.bus.connect(host, port, service_id,
                open_listener=partial(self.connection_opened, info),
                close_listener=partial(self.connection_closed, info),
                lock=self.lock)
    
    def disconnect_from(self, service_id):
        self.service_map[service_id].close()
        del self.service_map[service_id]
    
    def connection_opened(self, info, connection):
        with print_exceptions:
            if self.bind_function:
                self.bind_function(self, connection, info)
    
    def connection_closed(self, info, connection):
        with print_exceptions:
            if self.unbind_function:
                self.unbind_function(self, connection, info)
    
    @synchronized_on("lock")
    def close(self):
        if not self.is_alive: # Already closed
            return
        self.is_alive = False
        for service_id in list(self.service_map.keys()):
            self.disconnect_from(service_id)
        self.bus.remove_service_listener(self.service_event_wrapper)
    
    def __getitem__(self, name):
        return MultipleServiceFunction(self, name)
    
    @property
    def live_connections(self):
        with self.lock:
            for connection in list(self.service_map.values()):
                if connection.is_connected:
                    yield connection


class MultipleServiceFunction(object):
    def __init__(self, proxy, name):
        self.proxy = proxy
        self.name = name
    
    def __call__(self, *args, **kwargs):
        with self.proxy.lock:
            callback = kwargs.get("callback", autobus2.SYNC)
            timeout = kwargs.get("timeout", 30)
            safe = kwargs.get("safe", False)
            if callback is None:
                count = 0
                for connection in self.proxy.live_connections:
                    count += 1
                    connection[self.name](*args, callback=None, safe=safe)
                return count
            elif callback is autobus2.SYNC:
                # This is a bit more complicated. What we're going to do is
                # create one queue for each connection, then call the functions
                # on the connections with each queue's put method as the
                # callback.
                raise NotImplementedError
            else:
                count = 0
                for connection in self.proxy.live_connections:
                    count += 1
                    connection[self.name](*args, callback=partial(callback, connection.service_id), safe=safe)
                return count
                
        
    
















































