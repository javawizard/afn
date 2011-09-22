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
        net.OutputThread(socket, self.queue.get).start()
        net.InputThread(socket, self.received, self.cleanup).start()
    
    def received(self, message):
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
                    self.send(messaging.create_response(message))
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
        self.send(messaging.create_response(command_or_id), _error={"text": reason})

    def close(self):
        self.queue.put(None)
    
    def shutdown(self):
        self.socket.close()
        self.queue.put(None)
    
    def cleanup(self):
        self.socket.close()
    
    def process_call(self, message):
        function = self.service.functions.get(message["name"], None)
        if not function:
            self.send_error(message, "That function does not exist.")
            return
        args = message["args"]
        if function.mode == autobus2.SYNC:
            self.send(function.call(message, args))
        elif function.mode == autobus2.THREAD:
            Thread(target=lambda: self.send(function.call(message, args))).start()
        else:
            self.send(messaging.create_response(message, result=None))
            Thread(target=functools.partial(function.call, message, args)).start()


class LocalService(object):
    """
    A service created locally and published to remote clients. This is the
    class of objects returned from bus.create_service().
    
    The various create_* functions can be called on LocalService instances to
    create functions, events, and objects that are published remotely.
    """
    def __init__(self, bus, id, info):
        self.id = id
        self.info = info
        self.bus = bus
        self.functions = {}
        self.events = {}
        self.objects = {}
        # TODO: manually create autobus.objects, then create autobus.events and
        # autobus.functions the normal way
    
    def create_function(self, name, function, mode=None):
        if mode is None:
            mode = autobus2.THREAD
        function = LocalFunction(self, name, function, mode)
        self.functions[name] = function
    
    def create_event(self):
        pass
    
    def create_object(self):
        pass


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
            return messaging.create_response(message, exception={"text":
                                    "%s: %s" % (type(e).__name__, str(e))})


class LocalEvent(object):
    pass


class LocalObject(object):
    pass













































