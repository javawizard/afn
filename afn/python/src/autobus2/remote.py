"""
This module contains classes and functions relating to connecting to and using
remote services.
"""

from Queue import Queue, Empty
from autobus2 import net, messaging, exceptions, common
from utils import Suppress, print_exceptions, Consume
from threading import Thread, RLock, Condition
import json
import autobus2
from traceback import print_exc
import time
from concurrent import synchronized_on
from socket import socket as Socket, error as SocketError, timeout as SocketTimeout
from afn.utils import full_name
from afn.utils.multimap import Multimap
from afn.utils.listener import EventTable, PropertyTable


class Connection(common.AutoClose):
    """
    A connection to a remote service. This class allows for calling functions
    on the remote service, listening for events, and watching objects.
    
    Instances of this class should not be created directly; instead, a new Bus
    object should be created, and its connect function called. The connect
    function will then return an instance of this class.
    """
    def __init__(self, bus, host, port, service_id, timeout, open_listener, close_listener, fail_listener, lock=None):
        """
        Creates a new connection, given the specified parent bus and socket.
        This constructor sets up everything and then sends an initial message
        to the remote socket indicating what service is to be connected to.
        """
        # Incremented on __enter__, decremented on __exit__. Allows Connection
        # instances to be re-entrant context managers.
        self.context_enters = 0
        self.bus = bus
        # The socket the connection is connected on. This only gets assigned
        # once the connection has successfully bound to the remote service.
        self.socket = None
        # The socket that's currently being connected. This is assigned when
        # a connection has been established but the handshake hasn't yet
        # finished.
        self.would_be_socket = None
        # The host, port, and service id to connect to
        self.host = host
        self.port = port
        self.service_id = service_id
        # The queue of messages to send to the remote socket
        self.queue = None
        # A map of outstanding queries. When a message is sent that requires a
        # synchronous response, a function, usually another queue's put
        # function, is added to this map with its key being the message's id.
        # Responses targeted to the specified message id will be passed to the
        # function in question.
        self.query_map = {}
        if lock is None:
            lock = RLock() # TODO: use the bus's lock instead, or are we good
            # to use our own lock?
            # FIXME: Actually, I just realized that might fix an exception I
            # noted yesterday occurs relating to connections. This needs to be
            # investigated more.
        self.lock = lock
        # A condition that's notified whenever a connection attempt finishes,
        # whether or not it was successful.
        self.connect_condition = Condition(self.lock)
        # The number of attempts we've made to connect since this connection
        # was created, whether or not said attempts fail or succeed. This is
        # used mainly by wait_for_connect to detect if we've already attempted
        # to connect yet.
        self.connect_attempts = 0
        # Listeners that are notified when a socket connects and the handshake
        # completes, when the socket disconnects, and when an attempt to connect
        # and handshake fails, respectively.
        self.open_listener = open_listener
        self.close_listener = close_listener
        self.fail_listener = fail_listener
        # True if this connection is connected and has handshaked, false if it
        # has not.
        self.is_connected = False
        # True if this connection has not yet been closed.
        self.is_alive = True
        # PropertyTable mapping object names to their current values. The
        # functions self._start_watching and self._stop_watching are set as the
        # start watching/stop watching functions; these will send messages, if
        # the connection is connected, watching and unwatching the object. On
        # connection to the remote server, watched_names() is called to get a
        # list of names being watched; watches are then sent for these names.
        # NOTE: This property table MUST ONLY BE MODIFIED while synchronized on
        # the connection lock; if this isn't the case, things will break.
        self.objects = PropertyTable(self._start_watching, self._stop_watching)
#        net.OutputThread(socket, self.queue.get).start()
#        net.InputThread(socket, self.received, self.cleanup).start()
        # We query here so that an invalid service id will cause an exception
        # to be raised while constructing the service
        # self.query(messaging.create_command("bind", False, service=service_id), timeout=10)
        Thread(name="autobus2.remote.Connection._connect-initial", target=self._connect).start()
    
    def _connect(self):
        delay = 0.1
        delay_increment = 1.5
        delay_max = 20
        while self.is_alive:
            delay *= delay_increment
            if delay > delay_max:
                delay = delay_max
            with self.lock:
                if not self.is_alive:
                    return
            # Try to connect to the remote end
            s = Socket()
            s.settimeout(10) # TODO: make this configurable
            with self.lock:
                self.would_be_socket = s
            try:
                try:
                    s.connect((self.host, self.port))
                finally:
                    with self.lock:
                        self.would_be_socket = None
            except SocketError:
                # Connection failed; wait the specified delay, then start over
                net.shutdown(s)
                self._on_connection_failed()
                self.sleep_while_alive(delay)
                continue
            s.settimeout(None)
            # Create the queue holding messages to be sent to the remote end
            queue = Queue()
            input_thread, output_thread = net.start_io_threads(
                    s, None, queue.get,
                    name="remote.Connection %r" % self, input_daemon=True)
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
                self._on_connection_failed()
                self.sleep_while_alive(delay)
                continue
            # The connection succeeded and we've bound to the remote service
            # successfully, so we lock on ourselves and stick everything into
            # the relevant fields, and send any initial messages (initial
            # object listeners, event listeners, etc) that we need to.
            with self.lock:
                if not self.is_alive: # close() was called while we were
                    # binding, so we close everything and return
                    queue.put(None)
                    net.shutdown(s)
                    input_thread.function = lambda *args: None
                    return
                # Handshake completed; assign to socket
                self.socket = s
                self.is_connected = True
                # Assign this connection's queue
                self.queue = queue
                # Switch the input thread's function to self.received
                input_thread.function = self.received
                # Send initial messages and such
                self._on_connection_succeeded()
            return
    
    def sleep_while_alive(self, amount):
        """
        Checks every second, for up to amount seconds, to see if this
        connection is alive. This method returns once the connection has been
        closed or the specified number of seconds are up.
        """
        while amount > 0 and self.is_alive:
            time.sleep(amount%1.0)
            amount -= 1
    
    @synchronized_on("lock")
    def _on_connection_failed(self):
        """
        Called from the connect thread (_connect) when a connection attempt
        fails. This increments connection_attempts, notifies the connect
        condition, and calls the fail listener, if one was specified when
        constructing this connection.
        """
        self.connect_attempts += 1
        self.connect_condition.notify_all()
        with print_exceptions:
            if self.fail_listener:
                self.fail_listener(self)
    
    @synchronized_on("lock")
    def _on_connection_succeeded(self):
        """
        Called from the connect thread (_connect) when a connection attempt
        succeeds. This increments connect_attempts, notifies the connect
        condition, sends a watch message for all objects in
        self.object_watchers, and calls the open listener, if one was specified
        when constructing this connection.
        """
        self.connect_attempts += 1
        self.connect_condition.notify_all()
        # Send a watch command for all objects currently being watched in the
        # property table. The server will send changed notices on receiving our
        # watch request, so we don't need to worry about separately updating
        # the object's values.
        for name in self.objects.watched_names():
            self.send(messaging.notice("watch", name=name))
        # Notify the open listener, if one exists
        with print_exceptions:
            if self.open_listener:
                self.open_listener(self)

    def close(self):
        with self.lock:
            if not self.is_alive: # Already closed
                return
            if self.socket: # Shut down the socket, if it exists
                self.queue.put(None)
                net.shutdown(self.socket)
                self.socket = None
            if self.would_be_socket: # Shut down the would-be socket, if one
                # exists
                net.shutdown(self.would_be_socket)
            self.is_connected = False
            self.is_alive = False
    
    def cleanup(self):
        """
        Called by the... either the InputThread or the OutputThread, I'm
        forgetting which... after a successful connection attempt is being
        terminated. This can therefore be called multiple times, once for each
        successful connection attempt.
        """
        with self.lock:
            # Shut down the current socket
            if self.socket:
                net.shutdown(self.socket)
                # Shut down the output queue
                self.queue.put(None)
            self.is_connected = False
            # Notify all outstanding queries that the connection has been lost
            for f in self.query_map.copy().itervalues():
                with print_exceptions:
                    f(exceptions.ConnectionLostException())
            # Then clear all outstanding queries
            self.query_map.clear()
            # And then clear all known object values
            self.objects.clear()
            # Call the close listener, if one was specified when constructing
            # this connection
            with print_exceptions:
                if self.close_listener:
                    self.close_listener(self)
            # If the connection hasn't been closed, start a thread attempting
            # to reconnect
            if self.is_alive:
                Thread(name="autobus2.remote.Connection._connect-reconnect", target=self._connect).start()
    
    def send(self, message):
        # print "Sending: " + str(message)
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
        """
        Sends the specified message, then blocks until a response is received
        or the specified timeout has elapsed. This works by creating a queue,
        sending the message with send_async(message, queue.put), then waiting
        on the queue for 30 seconds. If nothing was placed on the queue after
        that many seconds, the query is canceled (via cancel_query) and a
        TimeoutException is raised. If a response that was an Exception
        instance was received, it is raised. Otherwise, the result is returned.
        """
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
        """
        Cancels the query corresponding to the specified id. If call is True
        (the default), the corresponding function will receive an instance of
        QueryCanceledException. If no such query exists, this function does
        nothing.
        """
        with self.lock:
            with Suppress(KeyError): # In case the query has already finished
                function = self.query_map[id]
                if call:
                    with print_exceptions:
                        function(exceptions.QueryCanceledException())
                del self.query_map[id]
    
    @synchronized_on("lock")
    def received(self, message):
        """
        Called by the connection's input thread when a message has been
        received. This function is in charge of dispatching the message to any
        send_async functions waiting for a response, and otherwise processing
        the message.
        """
        # print "Received: " + str(message)
        if message is None: # The input thread calls this function with None as
            # the argument when the socket closes, so we take the opportunity
            # to clean everything up.
            self.cleanup()
            return
        if message["_type"] == 2: # Response to a command we presumably sent
            # with query, so look up the function waiting for the response, if
            # one exists, and call it.
            f = self.query_map.get(message["_id"], None)
            if f:
                # There was a function, so remove it from the map of functions
                # waiting for responses
                del self.query_map[message["_id"]]
                with print_exceptions:
                    if message.get("_error"): # Error happened, so call the
                        # function with an exception
                        f(exceptions.CommandErrorException(message["_error"]["text"]))
                    else: # Response was successful; pass it into the function
                        f(message)
        if message["_type"] in [1, 3]: # Command or notification
            # TODO: consider merging this part of this class with
            # local.RemoteConnection since it's substantially the same. Maybe
            # even consider having the command/response system be its own layer
            # underneath this class, which would then function purely as the
            # layer on top of that that deals with Autobus-specific stuff.
            processor = getattr(self, "process_" + message["_command"], None)
            if not processor:
                print "Invalid message received and ignored. Command: %s" % message["_command"]
                return
            processor(message)
    
    def __getitem__(self, name):
        # Just return the function for now
        return Function(self, name)
    
    def _start_watching(self, name):
        """
        Called when the first listener for a particular named object is added
        to the objects property table. This just sends a watch command if we're
        connected.
        """
        with self.lock:
            if self.is_connected:
                self.send(messaging.notice("watch", name=name))
    
    def _stop_watching(self, name):
        """
        Called when the last listener for a particular named object is removed
        from the objects property table. This just sends an unwatch command if
        we're connected.
        """
        with self.lock:
            if self.is_connected:
                self.send(messaging.notice("unwatch", name=name))
    
    @synchronized_on("lock")
    def watch_object(self, name, function):
        """
        Watches the object with the specified name. When changes to the object
        happen, the specified function will be called. Its signature is
        function(name, old, new), where name is the name of the object (the
        same name passed into this method), old is the old value of the object,
        and new is the new value of the object.
        
        This method can be called at any time (regardless of whether the
        connection is currently connected), and it can be called multiple times.
        When a connection is disconnected, or when the corresponding object
        doesn't actually exist, the object's value will appear to be None.
        
        Note that when the function is called, it will be called synchronously
        on the connection thread. It must therefore return quickly, or spawn a
        thread if a longer task needs to be completed.
        """
        self.objects.watch(name, function)
    
    @synchronized_on("lock")
    def unwatch_object(self, name, function):
        """
        Removes the watch on the specified object. The specified function will
        be synchronously called with None as the new value of the object.
        
        If multiple watches were placed on the same object, the other watches
        will continue to function correctly.
        """
        self.objects.unwatch(name, function)
            
    def process_changed(self, message):
        """
        Called when a message was received from the server indicating that a
        particular object changed. This sets the object's new value into the
        objects property table; the property table will take care of notifying
        watchers added with watch_object.
        """
        if isinstance(message, exceptions.ConnectionLostException):
            return
        name = message["name"]
        value = message["value"]
        if value is None:
            with Suppress(KeyError):
                del self.objects[name]
        else:
            self.objects[name] = value
    
    def __str__(self):
        return "<%s to %s:%s service_id=%s is_connected=%s is_alive=%s>" % (
                full_name(self), self.host, self.port, self.service_id,
                self.is_connected, self.is_alive)
    
    __repr__ = __str__
    
    def wait_for_connect(self, timeout=10):
        """
        Waits until this connection has made at least one attempt since it was
        first created to connect. If the attempt succeeds, this function
        returns. If the attempt fails, this function throws an exception.
        
        If the connection's attempt to connect does not complete by the given
        timeout, an exception will be thrown.
        """
        with self.lock:
            if self.connect_attempts == 0:
                self.connect_condition.wait(timeout)
            if self.connect_attempts == 0:
                raise exceptions.TimeoutException()
            if not self.is_connected:
                raise exceptions.ConnectionException()
            


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
            command = messaging.command("call", name=self.name, args=list(args))
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
                # Then we send with the wrapper
                self.connection.send_async(command, wrapper)
    
    def __str__(self):
        return "<%s %s from %s>" % (full_name(self), self.name, self.connection)
    
    __repr__ = __str__




































