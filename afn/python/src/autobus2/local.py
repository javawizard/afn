"""
This module contains classes and functions relating to publishing services.
"""

from Queue import Queue, Empty

from autobus2 import net, messaging, exceptions
import autobus2
from utils import no_exceptions
from traceback import print_exc
from threading import Thread
import functools
from afn.utils.concurrent import synchronized_on
from afn.utils.multimap import Multimap as _Multimap
import itertools

class RemoteConnection(object):
    """
    A connection from a remote client attached to a particular local service.
    This is for Autobus's use only; external code won't ever need to see this.
    
    The class that external code receives instances of when connecting to a
    service is Connection, not RemoteConnection. When a Connection connects,
    the other end of the socket, the one on the listening end, will be a
    RemoteConnection.
    """
    def __init__(self, bus, socket):
        self.bus = bus
        self.socket = socket
        self.queue = Queue()
        self.service = None
        self.watched_objects = set()
        self.listened_events = set()
        net.OutputThread(socket, self.queue.get).start()
        net.InputThread(socket, self.received).start()
    
    def received(self, message):
        if message is None: # Connection lost
            self.cleanup()
            return
        try:
            if self.service is None: 
                if message["_command"] != "bind":
                    raise Exception("First message must be bind")
                name = message["service"]
                with self.bus.lock:
                    service = self.bus.local_services.get(name)
                    if not service:
                        self.send_error(message, "The service you attempted to bind to does not exist.")
                        self.close()
                        return
                    self.service = service
                    # The response includes the service since specifying None
                    # as the service causes the introspection service to be
                    # bound to, and this lets the client know what the
                    # introspection service's service id currently is
                    self.send(messaging.create_response(message, service=name))
                    return
            processor = getattr(self, "process_" + message["_command"], None)
            if not processor:
                self.send_error(message, "That command does not exist.")
            processor(message)
        except:
            print_exc()
            with no_exceptions:
                self.send_error(message, "An exception was thrown while processing that command.")
    
    def send(self, message):
        if message:
            self.queue.put(message)
    
    def send_error(self, command_or_id, reason):
        self.send(messaging.create_error(command_or_id, reason))

    def close(self):
        self.queue.put(None)
        net.shutdown(self.socket)
    
    def cleanup(self):
        with self.bus.lock:
            self.queue.put(None)
            self.bus.bound_connections.remove(self)
            net.shutdown(self.socket)
            if self.service:
                for name in self.watched_objects:
                    self.service.unwatch_object(self, name)
                for name in self.listened_events:
                    self.service.unlisten_for_event(self, name)
    
    def process_call(self, message):
        function = self.service.functions.get(message["name"])
        if not function:
            self.send_error(message, "That function does not exist.")
            return
        args = message["args"]
        if function.mode == autobus2.SYNC:
            self.send(function.call(message, args))
        elif function.mode == autobus2.THREAD:
            Thread(name="autobus2.local.RemoteConnection-function-caller", 
                    target=lambda: self.send(function.call(message, args))).start()
        else:
            self.send(messaging.create_response(message, result=None))
            Thread(name="autobus2.local.RemoteConnection-function-async-caller", 
                    target=functools.partial(function.call, message, args)).start()
    
    def process_watch(self, message):
        with self.bus.lock:
            name = message["name"]
            if name in self.watched_objects:
                self.send_error(message, "You're already watching that object.")
                return
            self.watched_objects.add(name)
            self.service.watch_object(self, name)
    
    def process_listen(self, message):
        raise NotImplementedError
    
    def process_unwatch(self, message):
        raise NotImplementedError
    
    def process_unlisten(self, message):
        raise NotImplementedError
    
    def process_ping(self, message):
        self.send(messaging.create_response(message))


class LocalService(object):
    """
    A service created locally and published to remote clients. This is the
    class of objects returned from bus.create_service().
    
    The various create_* functions can be called on LocalService instances to
    create functions, events, and objects that are published remotely.
    
    The function activate() can be called for services created by
    bus.create_service(..., active=False). This causes the service to become
    active. Inactive services are hidden from the outside world; this allows
    all of the functions needed for a particular service to be set up before
    it's published to the network.
    """
    def __init__(self, bus, id, info):
        self.id = id
        self.info = info
        self.bus = bus
        self.active = False
        self.functions = {}
        self.events = {}
        self.objects = {}
        self.event_listeners = _Multimap() # Maps event names to
        # RemoteConnection instances listening for them
        self.object_watchers = _Multimap() # Ditto but for object watches
    
    def activate(self):
        with self.bus.lock:
            if self.active: # Already active
                return
            self.active = True
            for publisher in self.bus.publishers:
                publisher.add(self)
    
    def create_function(self, name, function, mode=None):
        if mode is None:
            mode = autobus2.THREAD
        function = LocalFunction(self, name, function, mode)
        self.functions[name] = function
    
    def create_event(self):
        raise NotImplementedError
    
    def create_object(self, name, value):
        object = LocalObject(self, name, value)
        self.objects[name] = object
        object.notify_created()
    
    def use_py_object(self, py_object):
        for name in dir(py_object):
            if name.startswith("_"):
                continue
            value = getattr(py_object, name)
            if not callable(value):
                continue
            self.create_function(name, value)
    
    def watch_object(self, connection, name):
        self.object_watchers.add(name, connection)
    
    def unwatch_object(self, connection, name):
        self.object_watchers.remove(name, connection)


class LocalFunction(object):
    def __init__(self, service, name, function, mode):
        self.service = service
        self.name = name
        self.function = function
        self.mode = mode
    
    def call(self, message, args):
        """
        Calls this function and returns a response for the specified message,
        which should be a call command.
        """
        try:
            result = self.function(*args)
            return messaging.create_response(message, result=result)
        except Exception as e:
            return messaging.create_error(message, "Remote function threw an exception: %s: %s" % (type(e), e))


class LocalEvent(object):
    pass


class LocalObject(object):
    def __init__(self, service, name, value):
        net.ensure_jsonable(value)
        self.service = service
        self.name = name
        self.value = value
    
    def set_value(self, value):
        net.ensure_jsonable(value)
        with self.service.bus.lock:
            self.value = value
            for watcher in self.service.object_watchers.get(self.name, []):
                watcher.send(messaging.create_command("changed", True, name=self.name, value=value))
    
    def notify_created(self):
        with self.service.bus.lock:
            for watcher in self.service.object_watchers.get(self.name, []):
                watcher.send(messaging.create_command("changed", True, name=self.name, value=self.value))















































