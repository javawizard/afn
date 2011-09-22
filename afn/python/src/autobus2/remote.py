"""
This module contains classes and functions relating to connecting to and using
remote services.
"""

from Queue import Queue, Empty
from autobus2 import net, messaging, exceptions
from utils import Suppress
from threading import RLock


class Connection(object):
    """
    A connection to a remote service. This class allows for calling functions
    on the remote service, listening for events, and watching objects.
    
    Instances of this class should not be created directly; instead, a new Bus
    object should be created, and its connect function called.
    """
    def __init__(self, bus, socket, service_id):
        """
        Creates a new connection, given the specified parent bus and socket.
        This constructor sets up everything and then sends an initial message
        to the remote socket indicating what service is to be connected to.
        """
        self.bus = bus
        self.socket = socket
        self.service_id = service_id
        self.queue = Queue()
        self.query_map = {}
        self.query_lock = RLock()
        self.is_connected = True
        net.OutputThread(socket, self.queue.get).start()
        net.InputThread(socket, self.received, self.cleanup).start()
        self.send(messaging.create_command("bind", True, service=service_id))

    def close(self):
        self.queue.put(None)
    
    def shutdown(self):
        self.queue.put(None)
        self.socket.close()
    
    def cleanup(self):
        self.socket.close()
        self.is_connected = False
        for queue in self.query_map.copy().itervalues():
            queue.put(None)
    
    def send(self, message):
        if message:
            self.queue.put(message)
    
    def send_async(self, message, callback):
        """
        Sends the specified message. The specified callback will be called with
        the response.
        
        Unless the command isn't responded to by the remote side of the
        connection, the specified callback is guaranteed to be called. If the
        connection is not currently connected, a NotConnectedException will be
        raised. If the connection disconnects while waiting for a response, the
        callback will be called, passing in None.
        """
    
    def query(self, message, timeout=30):
        q = Queue()
        self.query_map[message["_id"]] = q.put
        self.send(message)
        try:
            response = q.get(timeout=timeout)
            with Suppress(KeyError):
                del self.query_map[message["_id"]]
            if response is None: # Connection lost while waiting for a response
                raise exceptions.ConnectionLostException()
            return response
        except Empty: # Timeout while waiting for response
            with Suppress(KeyError):
                del self.query_map[message["_id"]]
            raise exceptions.TimeoutException()
    
    def received(self, message):
        if message["_type"] == 2: # response
            f = self.query_map.get(message["_id"], None)
            if f:
                f(message)
        if message["_type"] in [1, 3]:
            command = message["_command"]
            # TODO: add things for processing change and fire commands
            print "Invalid message received and ignored. Command: %s" % command


class Function(object):
    """
    An object representing a remote function. Instances of this class are
    callable; calling them will invoke the remote function.
    
    Instances of this class can be obtained by doing
    some_connection["function_name"].
    """
    def __init__(self, connection, name):
        self.connection = connection
        self.name = name
    
    def __call__(self):
        pass




































