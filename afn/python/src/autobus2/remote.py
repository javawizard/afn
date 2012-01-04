"""
This module contains classes and functions relating to connecting to and using
remote services.
"""

from Queue import Queue, Empty
from autobus2 import net, messaging, exceptions
from utils import Suppress
from threading import Thread, RLock
import json
import autobus2
from utils import print_exceptions
from traceback import print_exc
import time
from socket import socket as Socket, error as SocketError, timeout as SocketTimeout


class Connection(object):
    """
    A connection to a remote service. This class allows for calling functions
    on the remote service, listening for events, and watching objects.
    
    Instances of this class should not be created directly; instead, a new Bus
    object should be created, and its connect function called. The connect
    function will then return an instance of this class.
    """
    def __init__(self, bus, host, port, service_id, timeout, open_listener, close_listener):
        """
        Creates a new connection, given the specified parent bus and socket.
        This constructor sets up everything and then sends an initial message
        to the remote socket indicating what service is to be connected to.
        """
        self.context_enters = 0
        self.bus = bus
        self.socket = None
        self.host = host
        self.port = port
        self.service_id = service_id
        self.queue = Queue()
        self.query_map = {}
        self.lock = RLock()
        self.open_listener = open_listener
        self.close_listener = close_listener
        self.is_connected = False
        self.is_alive = True
#        net.OutputThread(socket, self.queue.get).start()
#        net.InputThread(socket, self.received, self.cleanup).start()
        # We query here so that an invalid service id will cause an exception
        # to be raised while constructing the service
        # self.query(messaging.create_command("bind", False, service=service_id), timeout=10)
        Thread(target=self._connect).start()
    
    def _connect(self):
        delay = 0.1
        delay_increment = 1.5
        delay_max = 20
        while True:
            delay *= delay_increment
            if delay > delay_max:
                delay = delay_max
            s = Socket()
            try:
                s.connect((self.host, self.port))
            except SocketError:
                time.sleep(delay)
                continue
            queue = Queue()
            input_thread = net.InputThread(s, None)
            input_thread.start()
            output_thread = net.OutputThread(s, queue.get)
            output_thread.start()
            input_queue = Queue()
            queue.put(messaging.create_command("bind", service=self.service_id))
            def received(message):
                input_thread.function = None
                input_queue.put(message)
            input_thread.function = received
            try:
                bind_response = input_queue.get(timeout=10) # TODO: make timeout configurable
            except Empty:
                bind_response = None
            if bind_response is None 
            # self.socket = s
            # self.is_connected = True
            # TODO: finish this up
            return

    def close(self):
        self.queue.put(None)
        net.shutdown(self.socket)
    
    def cleanup(self):
        with self.query_lock:
            self.socket.close()
            self.queue.put(None)
            self.is_connected = False
            for f in self.query_map.copy().itervalues():
                with print_exceptions:
                    f(None)
        with print_exceptions:
            if self.close_listener:
                self.close_listener()
    
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
        callback will be called, passing in an instance of
        ConnectionLostException. If some other error occurs while processing,
        a suitable exception will be created and passed into the callback.
        
        The specified function will be called on the input thread for this
        connection, so it must not block for a significant amount of time; if
        it does, it will freeze up receiving of messages for this connection.
        """
        if not message:
            raise exceptions.NullMessageException
        with self.query_lock:
            if not self.is_connected:
                raise exceptions.NotConnectedException
            def wrapper(response):
                if not response:
                    callback(exceptions.ConnectionLostException())
                if response.get("_error", None):
                    callback(exceptions.CommandErrorException(response["_error"]["text"]))
                else:
                    callback(response)
            self.query_map[message["_id"]] = wrapper
        self.send(message)
    
    def query(self, message, timeout=30):
        if message is None:
            raise exceptions.NullMessageException
        q = Queue()
        with self.query_lock:
            if not self.is_connected:
                raise exceptions.NotConnectedException
            self.query_map[message["_id"]] = q.put
        self.send(message)
        try:
            response = q.get(timeout=timeout)
            with Suppress(KeyError):
                with self.query_lock:
                    del self.query_map[message["_id"]]
            if response is None: # Connection lost while waiting for a response
                raise exceptions.ConnectionLostException()
            if response.get("_error", None):
                raise exceptions.CommandErrorException(response["_error"]["text"])
            return response
        except Empty: # Timeout while waiting for response
            with Suppress(KeyError):
                with self.query_lock:
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
    
    def __getitem__(self, name):
        return Function(self, name)
    
    def __enter__(self):
        self.context_enters += 1
        return self
    
    def __exit__(self, *args):
        self.context_enters -= 1
        if self.context_enters == 0:
            self.close()


class ConnectionManager(object):
    def __init__(self, bus, connect_listener=None, disconnect_listener=None):
        self.bus = bus
    
    def connect_to(self, host, port, service_id):
        Thread(target=self._connect_to, args=(host, port, service_id)).start()
    
    def _connect_to(self, host, port, service_id):
        while True:
            try:
                connection = self.bus.connect(host, port, service_id)
            except (exceptions.ConnectionException, exceptions.TimeoutException):
                pass
            except:
                print_exc()
    
    def disconnect_from(self, host, port, service_id):
        pass
    
    @property
    def connections(self):
        """
        A list of connections that this ConnectionManager is currently
        connected to.
        """
        pass


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
    
    def __call__(self, *args, **kwargs):
        """
        Calls this function. The positional arguments passed to this call are
        passed to the function on the remote side when it is invoked. They must
        be JSON-encodable values; an InvalidValueException will be thrown if
        one of them is not.
        
        Two keyword arguments can be passed when calling a function:
        
        callback: This is autobus2.SYNC to call the function synchronously
        (which is what most people expect; the call will block until the remote
        service sends back a response for the function call, at which point it
        will be returned), None to call the function but return immediately
        without waiting for a response (the return value will be None), or a
        one-argument function (or other Python callable), which will cause this
        call to return immediately and the specified function to be invoked
        with the response once it arrives. The default, if callback is not
        specified, is SYNC.
        
        timeout: This only has any effect if SYNC is used as the callback (or
        the callback is not specified, since it defaults to SYNC). If the
        remote service has not sent a response in this many seconds, the call
        will stop immediately and throw a TimeoutException. The default, if
        timeout is not specified, is 30.
        
        If a callback is used and an exception happens while processing (or if
        the remote function throws an exception), the exception object itself
        is passed into the callback. If a callback is not used, the exception
        will be raised instead.
        """
        for a in args:
            try:
                json.dumps(a)
            except:
                raise exceptions.InvalidValueException
        callback = kwargs.get("callback", autobus2.SYNC)
        timeout = kwargs.get("timeout", 30)
        command = messaging.create_command("call", name=self.name, args=list(args))
        if callback is autobus2.SYNC:
            result = self.connection.query(command, timeout)
            if result.get("exception"):
                raise exceptions.RemoteUserException(result["exception"]["text"])
            return result["result"]
        elif callback is None:
            command["_type"] = 3 # Change to a notice
            self.connection.send(command)
        else:
            def wrapper(response):
                if isinstance(response, dict): # Normal response
                    if response.get("exception"): # Remote function threw an exception
                        callback(exceptions.RemoteUserException(result["exception"]["text"]))
                    else: # Remote function returned normally
                        callback(response["result"])
                else: # Some other exception while processing
                    callback(response)
            self.connection.send_async(command, wrapper)
            




































