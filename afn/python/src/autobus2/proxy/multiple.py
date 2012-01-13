
from threading import RLock, Condition
import autobus2
from autobus2 import exceptions, common
from afn.utils import wrap, print_exceptions
from afn.utils.concurrent import synchronized_on
from functools import partial
from afn.utils.multimap import Multimap as _Multimap

class MultipleServiceProxy(common.AutoClose):
    def __init__(self, bus, info_filter, bind_function=None, unbind_function=None):
        self.bus = bus
        self.info_filter = info_filter
        self.lock = RLock()
        self.bind_condition = Condition(self.lock)
        self.bind_function = bind_function
        self.unbind_function = unbind_function
        self.service_map = {} # Map of service ids to connections
        self.object_watchers = _Multimap()
        self.is_alive = True
        bus.add_service_listener(self.service_event, info_filter, True)
    
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
        connection = self.bus.connect(host, port, service_id,
                open_listener=partial(self.connection_opened, info),
                close_listener=partial(self.connection_closed, info),
                lock=self.lock)
        self.service_map[service_id] = connection
    
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
        self.bus.remove_service_listener(self.service_event)
    
    def __getitem__(self, name):
        return MultipleServiceFunction(self, name)
    
    @property
    def live_connections(self):
        with self.lock:
            for connection in list(self.service_map.values()):
                if connection.is_connected:
                    yield connection
    
    @synchronized_on("lock")
    def watch_object(self, name, function):
        """
        function -> (connection, info, value) -> None
        """
        self.object_watchers.add(name, function)
    
    @synchronized_on("lock")
    def unwatch_object(self, name, function):
        """
        """
        self.object_watchers.remove(name, function)
    
    def object_changed(self, name, connection, info, value):
        for watcher in self.object_watchers.get()


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
                
