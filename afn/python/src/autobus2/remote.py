"""
This module contains classes and functions relating to connecting to and using
remote services.
"""

from Queue import Queue, Empty
from autobus2 import net, messaging, exceptions
from utils import Suppress
from threading import Thread, RLock, Condition
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
        self.queue = None
        self.query_map = {}
        self.lock = RLock()
        self.connect_condition = Condition(self.lock)
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
            with self.lock:
                if not self.is_alive:
                    return
            # Try to connect to the remote end
            s = Socket()
            try:
                s.connect((self.host, self.port))
            except SocketError:
                # Connection failed; wait the specified delay, then start over
                time.sleep(delay)
                continue
            # Create the queue holding messages to be sent to the remote end
            queue = Queue()
            input_thread, output_thread = net.start_io_threads(s, None, queue.get)
            # Create the queue that will be used to hold the response to the
            # initial bind command that we're going to send
            input_queue = Queue()
            # Send the initial bind command
            queue.put(messaging.create_command("bind", service=self.service_id))
            # Then register a function waiting to receive the response
            def received(message):
                # Remove ourselves so that we only receive one message, namely
                # the bind response
                input_thread.function = None
                # Then go stuff the response in the queue
                input_queue.put(message)
            input_thread.function = received
            try:
                bind_response = input_queue.get(timeout=10) # TODO: make timeout configurable
            except Empty:
                bind_response = None
            if bind_response is None or bind_response.get("_error") is not None:
                # An error happened while getting the response, so close the
                # output thread, then forcibly close the socket, then put an
                # empty function into the input thread so that it wil die if
                # any other messages were received, then sleep the amount of
                # time we're supposed to and continue
                queue.put(None)
                net.shutdown(s)
                input_thread.function = lambda *args: None
                time.sleep(delay)
                continue
            # The connection succeeded and we've bound to the remote service
            # successfully, so we lock on ourselves and stick everything into
            # the relevant fields, and send any initial messages (initial
            # object listeners, event listeners, etc) that we need to.
            with self.lock:
                if not self.is_alive:
                    queue.put(None)
                    net.shutdown(s)
                    input_thread.function = lambda *args: None
                    return
                self.socket = s
                self.is_connected = True
                self.queue = queue
                input_thread.function = self.received
                with print_exceptions:
                    if self.open_listener:
                        self.open_listener(self)
                self.connect_condition.notify_all()
                # FIXME: send initial messages here, once we have any to send

    def close(self):
        with self.lock:
            if not self.is_alive: # Already closed
                return
            if self.socket:
                self.queue.put(None)
                net.shutdown(self.socket)
            self.is_connected = False
            self.is_alive = False
    
    def cleanup(self):
        with self.lock:
            if self.socket:
                net.shutdown(self.socket)
                self.queue.put(None)
            self.is_connected = False
            for f in self.query_map.copy().itervalues():
                with print_exceptions:
                    f(None)
            self.query_map.clear()
            with print_exceptions:
                if self.close_listener:
                    self.close_listener(self)
            if self.is_alive:
                Thread(target=self._connect).start()
    
    def send(self, message):
        with self.lock:
            if message:
                if self.is_connected:
                    self.queue.put(message)
                else:
                    raise exceptions.NotConnectedException
    
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
        with self.lock:
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
        with self.lock:
            if not self.is_connected:
                raise exceptions.NotConnectedException
            self.query_map[message["_id"]] = q.put
            self.send(message)
        try:
            response = q.get(timeout=timeout)
            with Suppress(KeyError):
                with self.lock:
                    del self.query_map[message["_id"]]
            if response is None: # Connection lost while waiting for a response
                raise exceptions.ConnectionLostException()
            if response.get("_error", None):
                raise exceptions.CommandErrorException(response["_error"]["text"])
            return response
        except Empty: # Timeout while waiting for response
            with Suppress(KeyError):
                with self.lock:
                    del self.query_map[message["_id"]]
            raise exceptions.TimeoutException()
    
    def received(self, message):
        if message is None:
            self.cleanup()
            return
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
    
    def wait_for_connect(self, timeout=None):
        with self.lock:
            if self.is_connected:
                return
            self.connect_condition.wait(timeout)


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
        # Make sure all the arguments can be converted into JSON correctly
        for a in args:
            try:
                json.dumps(a)
            except:
                raise exceptions.InvalidValueException
        callback = kwargs.get("callback", autobus2.SYNC)
        timeout = kwargs.get("timeout", 30)
        # Create the command to call the function
        command = messaging.create_command("call", name=self.name, args=list(args))
        if callback is autobus2.SYNC:
            # Synchronous call, so we query for the command
            result = self.connection.query(command, timeout)
            if result.get("exception"):
                raise exceptions.RemoteUserException(result["exception"]["text"])
            return result["result"]
        elif callback is None:
            # Asynchronous call; send the command as a notice and then return
            command["_type"] = 3
            self.connection.send(command)
        else:
            # Call with a callback, so we write a wrapper to handle everything
            def wrapper(response):
                if isinstance(response, dict): # Normal response
                    if response.get("exception"): # Remote function threw an exception
                        callback(exceptions.RemoteUserException(result["exception"]["text"]))
                    else: # Remote function returned normally
                        callback(response["result"])
                else: # Some other exception while processing
                    callback(response)
            self.connection.send_async(command, wrapper)
            




































