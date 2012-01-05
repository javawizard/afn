"""
This module contains classes and functions relating to connecting to and using
remote services.
"""

from Queue import Queue, Empty
from autobus2 import net, messaging, exceptions
from utils import Suppress, print_exceptions, Consume
from threading import Thread, RLock, Condition
import json
import autobus2
from traceback import print_exc
import time
from concurrent import synchronized_on
from socket import socket as Socket, error as SocketError, timeout as SocketTimeout
from afn.utils import full_name


class Connection(object):
    """
    A connection to a remote service. This class allows for calling functions
    on the remote service, listening for events, and watching objects.
    
    Instances of this class should not be created directly; instead, a new Bus
    object should be created, and its connect function called. The connect
    function will then return an instance of this class.
    """
    def __init__(self, bus, host, port, service_id, timeout, open_listener, close_listener, lock=None):
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
        if lock is None:
            lock = RLock()
        self.lock = lock
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
        Thread(name="autobus-initial-connect-thread", target=self._connect).start()
    
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
                if self.open_listener:
                    with print_exceptions:
                        self.open_listener(self)
                # FIXME: send initial messages here, once we have any to send
            return

    def close(self):
        with self.lock:
            if not self.is_alive: # Already closed
                return
            if self.socket:
                self.queue.put(None)
                net.shutdown(self.socket)
                self.socket = None
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
                    f(exceptions.ConnectionLostException())
            self.query_map.clear()
            with print_exceptions:
                if self.close_listener:
                    self.close_listener(self)
            if self.is_alive:
                Thread(name="autobus-reconnect-thread", target=self._connect).start()
    
    def send(self, message):
        with self.lock:
            if message:
                if self.is_connected:
                    self.queue.put(message)
                else:
                    raise exceptions.NotConnectedException
    
    def send_async(self, message, callback, safe=False):
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
        
        This function (send_async) returns message["_id"]. You can later on
        pass this to cancel_query to cancel this asynchronous command if you
        want.
        
        If safe is True, send_async won't throw any exceptions, not even
        NotConnectedException or NullMessageException; these will instead
        result in the callback being synchronously called with the exception
        as its only argument.
        """
        with Consume(exceptions.AutobusException, callback, safe):
            if not message:
                raise exceptions.NullMessageException
            with self.lock:
                if not self.is_connected:
                    raise exceptions.NotConnectedException
                self.query_map[message["_id"]] = callback
                self.send(message)
    
    def query(self, message, timeout=30):
        q = Queue()
        self.send_async(message, q.put)
        try:
            response = q.get(timeout=timeout)
        except Empty: # Timeout while waiting for response
            self.cancel_query(message["_id"], False)
            raise exceptions.TimeoutException()
        if isinstance(response, Exception):
            raise response
        else:
            return response
    
    def cancel_query(self, id, call=True):
        with self.lock:
            with Suppress(KeyError): # In case the query has already finished
                function = self.query_map[id]
                if call:
                    with print_exceptions:
                        function(exceptions.QueryCanceledException())
                del self.query_map[id]
    
    @synchronized_on("lock")
    def received(self, message):
        if message is None: # The input thread calls this function with None as
            # the argument when the socket closes
            self.cleanup()
            return
        if message["_type"] == 2: # response
            f = self.query_map.get(message["_id"], None)
            if f:
                del self.query_map[message["_id"]]
                with print_exceptions:
                    if message.get("_error"):
                        f(exceptions.CommandErrorException(message["_error"]["text"]))
                    else:
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
    
    def __str__(self):
        return "<%s to %s:%s service_id=%s is_connected=%s is_alive=%s>" % (
                full_name(self), self.host, self.port, self.service_id,
                self.is_connected, self.is_alive)
    
    __repr__ = __str__


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
        
        Three keyword arguments can be passed when calling a function:
        
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
        
        safe: This only has any effect if a function is used as the callback.
        When safe is True, no exceptions whatsoever will be thrown; instead,
        they will be synchronously passed into the callback. If safe is False
        (the default), some exceptions (such as a function argument that can't
        be converted to JSON being passed in or the connection being currently
        disconnected) will result in an exception being immediately thrown,
        while others (such as the remote function throwing an exception, or the
        connection going down before the remote function has returned) will
        result in the callback being called with the exception as its argument.
        
        If a callback is used and an exception happens while processing (or if
        the remote function throws an exception), the exception object itself
        is passed into the callback. If a callback is not used, the exception
        will be raised instead.
        """
        callback, timeout, safe = kwargs.get("callback", autobus2.SYNC), kwargs.get("timeout", 30), kwargs.get("safe", False)
        with Consume(exceptions.AutobusException, callback, safe):
            # Make sure all the arguments can be converted into JSON correctly
            net.ensure_jsonable(args)
            # Create the command to call the function
            command = messaging.create_command("call", name=self.name, args=list(args))
            if callback is autobus2.SYNC: # Synchronous call, so we query for the
                # command
                return self.connection.query(command, timeout)["result"]
            elif callback is None:  # Asynchronous call; send the command as a
                # notice and then return
                self.connection.send(messaging.convert_to_notice(command))
            else: # Call with a callback, so we write a wrapper to handle everything
                def wrapper(response):
                    if isinstance(response, dict): # Normal response
                        callback(response["result"])
                    else: # Exception while processing
                        callback(response)
                self.connection.send_async(command, wrapper)
    
    def __str__(self):
        return "<%s %s from %s>" % (full_name(self), self.name, self.connection)
    
    __repr__ = __str__




































