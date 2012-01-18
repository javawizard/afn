
from threading import RLock, Condition
import autobus2
from autobus2 import exceptions, common
from afn.utils import wrap, print_exceptions
from afn.utils.partial import partial
from afn.utils.concurrent import synchronized_on
from afn.utils.multimap import Multimap as _Multimap
from Queue import Queue

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
        for name, watchers in self.object_watchers.items():
            for watcher in watchers:
                connection.watch_object(name, partial(watcher, connection, info))
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
        for info, connection in self.service_map.items():
            connection.watch_object(name, partial(function, connection, info))
    
    @synchronized_on("lock")
    def unwatch_object(self, name, function):
        """
        """
        self.object_watchers.remove(name, function)
        for info, connection in self.service_map.items():
            connection.unwatch_object(name, partial(function, connection, info))


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
            elif callback is not autobus2.SYNC:
                count = 0
                for connection in self.proxy.live_connections:
                    count += 1
                    connection[self.name](*args, callback=partial(callback, connection.service_id), safe=safe)
                return count
            # SYNC mode.
            # This is a bit more complicated. What we're going to do is
            # create one queue for each connection, then call the functions
            # on the connections with each queue's put method as the
            # callback.
            queues = dict((c.service_id, Queue()) for c in self.proxy.live_connections)
            for c in self.proxy.live_connections:
                c[self.name](*args, callback=queues[c.service_id].put, safe=True)
        # We drop out of self.proxy.lock so that we can block on the queues
        results = {}
        for service_id in queues:
            # FIXME: This causes the timeout to be applied once for each
            # service that we're actually contacting. What we need to do is
            # keep track of when we start blocking and then only block how much
            # time is left until the timeout would have expired.
            try:
                results[service_id] = queues[service_id].get(timeout=timeout)
            except Exception as e:
                results[service_id] = e
        # Scan through and accumulate any exceptions into a map
        exception_map = {}
        for service_id, value in results.items():
            if isinstance(value, Exception):
                exception_map[service_id] = value
        # Then, if there were any exceptions to accumulate, throw an exception
        # containing them all
        if exception_map:
            raise exceptions.CommandErrorException(exception_map)
        # No exception happened, so return the results
        return results

