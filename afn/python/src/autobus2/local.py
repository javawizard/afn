"""
This module contains classes and functions relating to publishing services.
"""

from Queue import Queue, Empty

from autobus2 import net, messaging, exceptions, common
import autobus2
from utils import no_exceptions
from traceback import print_exc
from threading import Thread
import functools
from afn.utils import Suppress
from afn.utils.concurrent import synchronized_on
from afn.utils.multimap import Multimap as _Multimap
import itertools
import copy

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
        # print "Received: " + str(message)
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
        # print "Sending " + str(message)
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
            # print "watch: ", message
            name = message["name"]
            if name in self.watched_objects:
                self.send_error(message, "You're already watching that object.")
                return
            self.watched_objects.add(name)
            self.service.watch_object(self, name)
            if self.service.objects.get(name):
                object_value = self.service.objects[name].value
            else:
                object_value = None
            self.send(messaging.create_response(message, name=name, value=object_value))
    
    def process_listen(self, message):
        raise NotImplementedError
    
    def process_unwatch(self, message):
        raise NotImplementedError
    
    def process_unlisten(self, message):
        raise NotImplementedError
    
    def process_ping(self, message):
        self.send(messaging.create_response(message))


class LocalService(common.AutoClose):
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
    def __init__(self, bus, id, info, doc):
        self.id = id
        self.info = info
        self.bus = bus
        self.doc = doc
        self.active = False
        self.is_alive = True
        self.functions = {}
        self.events = {}
        self.objects = {}
        self.event_listeners = _Multimap() # Maps event names to
        # RemoteConnection instances listening for them
        self.object_watchers = _Multimap() # Ditto but for object watches
    
    @synchronized_on("bus.lock")
    def activate(self):
        if self.active: # Already active
            return
        if not self.is_alive:
            raise exceptions.ClosedException("This LocalService has been closed.")
        self.active = True
        self.bus._i_update(self.id)
        for publisher in self.bus.publishers:
            publisher.add(self)
    
    @synchronized_on("bus.lock")
    def deactivate(self):
        if not self.active: # Not active
            return
        if not self.is_alive:
            return
        self.active = False
        for publisher in self.bus.publishers:
            publisher.remove(self)
        self.bus._i_update(self.id)
    
    @synchronized_on("bus.lock")
    def close(self):
        self.is_alive = False
        self.deactivate()
        self.bus._close_service(self)
    
    def create_function(self, name, function, mode=None, doc=""):
        with self.bus.lock:
            if mode is None:
                mode = autobus2.THREAD
            function = LocalFunction(self, name, function, mode, doc)
            self.functions[name] = function
            self.bus._i_update(self.id)
    
    def create_event(self):
        raise NotImplementedError
    
    def create_object(self, name, value, doc=""):
        with self.bus.lock:
            object = LocalObject(self, name, value, doc)
            self.objects[name] = object
            object.notify_created()
            self.bus._i_update(self.id)
            return object
    
    @synchronized_on("bus.lock")
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
    
    def _add_introspection(self):
        self.create_object("autobus.details",
                self._i_details_function, "Provides information about the "
                "various functions, objects, and such provided by this interface")
        # Creating this function will cause the above object to update, so we
        # don't need to manually update it.
        self.create_function("autobus.get_details", self._i_details_function,
                doc="Returns the current value of the autobus.details object.")
    
    @synchronized_on("bus.lock")
    def _i_details_function(self):
        details = {}   
        details["id"] = self.id         
        details["active"] = self.active
        details["doc"] = self.doc
        details["info"] = self.info
        functions = {}
        events = {}
        objects = {}
        details["functions"] = functions
        details["events"] = events
        details["objects"] = objects
        for name, function in self.functions.items():
            functions[name] = {"name": name, "doc": function.doc}
        for name, event in self.events.items():
            events[name] = {"name": name, "doc": event.doc}
        for name, object in self.objects.items():
            objects[name] = {"name": name, "doc": object.doc}
        return details


class LocalFunction(object):
    def __init__(self, service, name, function, mode, doc):
        if mode not in (autobus2.SYNC, autobus2.ASYNC, autobus2.THREAD):
            raise ValueError("Invalid mode: " + str(mode))
        self.service = service
        self.name = name
        self.function = function
        self.mode = mode
        self.doc = doc
    
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
    def __init__(self, service, name, value, doc):
        self._value = value
        net.ensure_jsonable(self.get_value())
        self.service = service
        self.name = name
        self.doc = doc
    
    @property
    def value(self):
        return self.get_value()
    
    @value.setter
    def value(self, new_value):
        self.set_value(new_value)
    
    def get_value(self):
        if callable(self._value):
            value = self._value()
        else:
            value = self._value
        return copy.copy(value) # TODO: do we need to copy the value when
        # returning it here? We might only need to copy it when originally
        # setting it...
    
    def set_value(self, value):
        net.ensure_jsonable(value() if callable(value) else value)
        if not callable(value):
            value = copy.copy(value) # Make a copy so that modifications to the
            # original object don't affect us
        with self.service.bus.lock:
            self._value = value
            new_value = self.get_value()
            for watcher in self.service.object_watchers.get(self.name, []):
                watcher.send(messaging.create_command("changed", True, name=self.name, value=new_value, event="changed"))
    
    def changed(self):
        """
        Causes this object to act as if its value had changed. This is really
        only useful when this object's value has been set to a function; this
        indicates that the return value of the function has changed.
        """
        with self.service.bus.lock:
            self.set_value(self._value)
    
    def notify_created(self):
        with self.service.bus.lock:
            for watcher in self.service.object_watchers.get(self.name, []):
                watcher.send(messaging.create_command("changed", True, name=self.name, value=self.get_value(), event="created"))
    
    def remove(self):
        with self.service.bus.lock:
            del self.service.objects[self.name]
            for watcher in self.service.object_watchers.get(self.name, []):
                watcher.send(messaging.create_command("changed", True, name=self.name, value=None, event="removed"))
            with Suppress(KeyError):
                del self.service.object_watchers[self.name]















































