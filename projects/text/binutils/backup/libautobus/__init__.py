
from __future__ import with_statement
import os
import platform
import sys

is_jython = platform.system() == "Java"

__doc__ = """\
This module is the Python client library for Autobus. It's the authoritative
Autobus client library; features generally appear here first and then get
ported to versions of libautobus written for other languages.

To use this module, you'll generally start with an instance of
AutobusConnection. See that class's documentation for more information on how
to use it.

This module includes some hacks to get it to work properly on Jython. In
particular, Google's Protocol Buffers, which libautobus uses, do not work
properly on Jython by default, but simply importing libautobus adds a fix to
get it to work. (In fact, other Jython applications that want to make use of
Protocol Buffers could import libautobus just to get protocol buffers working.)
However, there are some issues right now, namely that if you're going to be
using libautobus on Jython, you need to have the Java libautobus on your
classpath. The reason is that this module changes some of its classes to
implement interfaces in the Java version, and the Java version then makes use
of this module running under Jython. As long as the Java libautobus is on the
classpath, you can import and use this module successfully from Jython code.
"""

# Workaround to get libautobus to work on Jython. Jython does not have Python's
# built-in buffer function, which Google's protocol buffers stuff needs to run.
# If we're on Jython, then, we'll create our own equivalent of the buffer
# function and install it in builtins.
if is_jython:
    def jython_buffer(object, offset=None, size=None):
        if size is not None:
            return object[offset:offset + size]
        elif offset is not None:
            return object[offset:]
        return object[:]
    sys.builtins["buffer"] = jython_buffer #@UndefinedVariable

from threading import Thread, RLock
from struct import pack, unpack
try:
    from json import dumps, loads
except ImportError:
    from simplejson import dumps, loads
# import autobus_protobuf.autobus_pb2 as protobuf
from traceback import print_exc
from socket import error as SocketError, socket as Socket, SHUT_RDWR
from functools import update_wrapper
from Queue import Queue, Empty
from time import sleep
import inspect
from datetime import datetime
from message_types import *
if is_jython:
    from java.util import Map, List #@UnresolvedImport
    from org.python.core import Options #@UnresolvedImport
    from array import array
    from afn.libautobus import (AutobusConnection as AutobusConnectionSuper, #@UnresolvedImport
            InterfaceWrapper as InterfaceWrapperSuper, #@UnresolvedImport
            FunctionWrapper as FunctionWrapperSuper, #@UnresolvedImport
            EventWrapper as EventWrapperSuper, #@UnresolvedImport
            ObjectWrapper as ObjectWrapperSuper) #@UnresolvedImport
    Options.respectJavaAccessibility = False

# We want to allow some additional types beyond the normal Python types to be
# encoded, so we'll use a function to do the translation
if is_jython:
    def jython_encode_replace(object):
        if isinstance(object, Map):
            return dict(object)
        if isinstance(object, (List, array)):
            return list(object)
        return object
else:
    def jython_encode_replace(object):
        return object
if not is_jython:
    AutobusConnectionSuper = object
    InterfaceWrapperSuper = object
    EventWrapperSuper = object # test file
    FunctionWrapperSuper = object
    ObjectWrapperSuper = object

COMMAND = 1
RESPONSE = 2
NOTIFICATION = 3

DEFAULT_PORT = 28862

MESSAGE_SEPARATORS = ",", ":"

next_id = 1
next_id_lock = RLock()

def get_next_id():
    """
    Thread-safely increments next_id and returns the value it previously held.
    """
    global next_id
    with next_id_lock:
        the_id = next_id
        next_id += 1
    return the_id

def create_message(action, message_type=COMMAND, **kwargs):
    """
    Creates a new dictionary representing a message. The dict's action key will
    be set to the specified action, which should be one of the constants
    defined in libautobus.message_types. If the specified message type is an
    integer (as COMMAND, RESPONSE, and NOTIFICATION are), it is set as the
    dict's message_type attribute, and the dict's message_id attribute is set
    to a newly-generated id. Otherwise, message_type is assumed to be a dict
    representing a message that this one should be constructed in reply to, and
    this dict's message_type will be set to RESPONSE and its message_id will
    be set to the specified message's message_id. The new dict is then
    returned.
    
    One additional thing I added: if message_type is a message to reply to,
    but its message_type is NOTIFICATION, the returned dict will have its
    message_type set to None. Connection.send ignores such messages, which will
    properly result in a response not being sent for a notification without
    whatever code that may be using this function having any idea about it.
    
    Any additional keyword parameters to this function are set as attributes
    on the returned dict. 
    """
    message = {"action": action}
    if isinstance(message_type, dict):
        message["message_id"] = message_type["message_id"]
        if message_type["message_type"] == NOTIFICATION:
            message["message_type"] = None
        else:
            message["message_type"] = RESPONSE
    else:
        message["message_id"] = get_next_id()
        message["message_type"] = message_type
    message.update(kwargs)
    return message

def read_fully(socket, length):
    """
    Reads the specified number of bytes from the socket. The socket will be
    repeatedly read until this many bytes have been received. They will then
    be returned as a string. If the socket dies before this many bytes can be
    read, EOFError will be raised.
    """
    data = ""
    while len(data) < length:
#        print "Reading bytes..."
        try:
            new_data = socket.recv(length - len(data))
        except: # Sporadically happens if the socket dies
            new_data = ""
#        print str(len(new_data)) + " bytes received"
        if new_data == "":
            raise EOFError()
        data += new_data
    return data

class InputThread(Thread):
    """
    A thread that reads messages from a socket and calls a user-defined
    function with the received messages. When the connection dies, it closes
    the socket and calls another user-defined function.
    """
    def __init__(self, socket, message_function, close_function=None):
        Thread.__init__(self)
        self.socket = socket
        self.message_function = message_function
        self.close_function = close_function
    
    def run(self):
        try:
            while True:
#                print "Waiting for client message..."
                message_length = read_fully(self.socket, 4)
                message_length = unpack(">i", message_length)[0]
#                print "Receiving message with length " + str(message_length)
                message_data = read_fully(self.socket, message_length)
#                print "Message received, decoding..."
                message = loads(message_data)
#                print "Dispatching received message..."
                self.message_function(message)
        except EOFError:
            pass
        except:
            print_exc()
#        print "Input thread finished, closing socket..."
        self.socket.close()
        if self.close_function:
#            print "Invoking close_function..."
            self.close_function()
#        print "Input thread has shut down."


class OutputThread(Thread):
    """
    A thread that repeatedly calls a function which should return a message
    object to send. The function can (and generally should) block as needed.
    This thread then writes the message to the socket. When the socket dies,
    it will be closed. If the function returns None, the socket will be
    immediately closed and this thread will exit.
    
    finished_function will be called after every message has been successfully
    written out. It defaults to lambda: None.
    
    The read function can return either a Message object or a string. If it's
    the former, it will be converted to a string via the message's
    SerializeToString function. Messages are ordinarily returned as Message
    objects to prevent message serialization from blocking the event queue in
    Autobus, but in some cases (such as when a single message is to be sent to
    several recipients), it may be more efficient to pre-encode the message
    and send it as a string.
    """
    def __init__(self, socket, read_function, finished_function=lambda: None):
        Thread.__init__(self)
        self.socket = socket
        self.read_function = read_function
        self.finished_function = finished_function
    
    def run(self):
        try:
            while True:
#                print "Waiting for a message to send..."
                message = self.read_function()
                try:
#                print "Getting ready to send message " + str(message)
                    if message is None:
#                        print "Message was empty, shutting down the thread"
                        break
                    if isinstance(message, str):
                        message_data = message
                    else:
                        message_data = dumps(message, separators=MESSAGE_SEPARATORS)
#                    print "Sending encoded message length..."
                    self.socket.sendall(pack(">i", len(message_data)))
#                    print "Sending encoded message..."
                    self.socket.sendall(message_data)
                finally:
                    self.finished_function()
        except SocketError:
            pass
        except:
            print_exc()
        self.socket.close()

class NoSuchInterfaceException(Exception):
    def __init__(self, interface_name, info=None):
        self.interface_name = interface_name
        self.info = info
    
    def __str__(self):
        value = "No such interface: " + self.interface_name
        if self.info is not None:
            value += ", additional info: " + self.info
        return value

class NoSuchFunctionException(Exception):
    def __init__(self, function_name):
        self.function_name = function_name
    
    def __str__(self):
        return "No such function: " + self.function_name

class TimeoutException(Exception):
    pass

class NotConnectedException(Exception):
    pass

def discard_args(function):
    """
    Returns a function that calls the specified function, discarding all
    arguments passed into it. The specified function is invoked with no
    arguments.
    """
    def wrapper(*args, **kwargs):
        function()
    update_wrapper(wrapper, function)
    return wrapper

def merge_repeated(src, dest):
    """
    Merges all items in the repeated field src into the repeated field dest. 
    """
    for item in src:
        dest.add().MergeFrom(item)

def encode_object(object):
    """
    Encodes an object to its json-encoded form.
    """
    object = jython_encode_replace(object)
    if isinstance(object, (bool, int, long, float, basestring)):
        instance = object
    elif isinstance(object, datetime):
        instance = ["timestamp", {"year": object.year,
                "month": object.month, "day": object.day, "hour": object.hour,
                "minute": object.minute, "second": object.second,
                "millisecond": object.microsecond / 1000}]
    elif object is None:
        instance = ["null", None]
    elif isinstance(object, (list, tuple)):
        instance = ["list", [encode_object(o) for o in object]]
    elif isinstance(object, dict):
        keys = []
        values = []
        for key, value in object.items():
            keys.append(encode_object(key))
            values.append(encode_object(value))
        instance = ["map", [keys, values]]
    elif isinstance(object, Exception):
        instance = ["exception", {"text": type(object).__name__ + ": " + str(object)}]
    else:
        raise Exception("Invalid instance type to encode: " + 
                str(type(object)))
    return instance

def decode_object(instance):
    if isinstance(instance, (bool, int, long, float, basestring)):
        return instance
    # This is a custom type packed as a list with two items, so we'll unpack it
    instance_type, value = instance;
    if instance_type == "timestamp":
        return datetime(year=value["year"], month=value["month"],
                day=value["day"], hour=value["hour"],
                minute=value["minute"], second=value["second"],
                microsecond=value["millisecond"] * 1000)
    elif instance_type == "null":
        return None
    elif instance_type == "list":
        return [decode_object(o) for o in value]
    elif instance_type == "map":
        result = {}
        keys, values = value
        for key, value in zip(keys, values):
            result[decode_object(key)] = decode_object(value)
        return result
    elif instance_type == "exception":
        return Exception(value["text"])
    else:
        raise Exception("Invalid instance type to decode: " + 
                str(instance_type))

def get_function_doc(interface, function_name):
    function = getattr(interface, function_name)
    is_method_before = inspect.ismethod(function)
    while hasattr(function, "wrapped"):
        function = function.wrapped
    if is_method_before or inspect.ismethod(function):
        argspec = inspect.getargspec(function)
        # Remove the leading "self" argument
        argspec = (argspec[0][1:],) + argspec[1:]
        args = inspect.formatargspec(*argspec)
    elif inspect.isfunction(function):
        args = inspect.formatargspec(*inspect.getargspec(function))
    else:
        args = "(...)"
    doc = inspect.getdoc(function)
    if doc is None:
        doc = ""
    return function_name + args + "\n\n" + doc

def start_thread(function):
    """
    A function decorator that can be used on functions registered to
    interfaces. Such functions will be invoked in a new thread, but their
    return value will still be captured and sent back to the client that
    invoked the function. This allows the function to make additional autobus
    calls while it's running and return results based on those calls, but no
    guarantee is made that two invocations of the function will not run at the
    same time.
    
    The same effect can be achieved by setting the attribute start_thread to
    have the value True on the function object. Indeed, this is exactly what
    this decorator does.
    """
    function.start_thread = True
    return function

class InterfaceWrapper(InterfaceWrapperSuper):
    def __init__(self, connection, name):
        self.connection = connection
        self.name = name
    
    def __getattr__(self, attribute):
        return FunctionWrapper(self.connection, self, attribute)
    
    if is_jython:
        get_function = __getattr__
    
    def __str__(self):
        return "<InterfaceWrapper on interface " + self.name + ">"

class FunctionWrapper(FunctionWrapperSuper):
    def __init__(self, connection, interface, name):
        self.connection = connection
        self.interface = interface
        self.name = name
    
    def __call__(self, *args):
        instance_args = [encode_object(arg) for arg in args]
        message = create_message(CallFunctionCommand,
                interface_name=self.interface.name, function=self.name,
                arguments=instance_args)
        response = self.connection.query(message)
        if response["action"] == ErrorResponse:
            raise Exception("Server-side error: " + response.get("text",
                    "No additional error information was sent by the server."))
        result = decode_object(response["return_value"])
        if isinstance(result, Exception):
            raise result
        return result
    
    def invoke(self, args):
        """
        Calls this function, passing in the arguments in the specified sequence
        object. This is currently equivalent to self(*args), and is present
        mostly to make using libautobus from Jython easier.
        """
        return self(*args)

    def invoke_py(self, args):
        """
        Exactly the same as invoke(args). The Java version of libautobus that
        wraps this function returns a PyObject instead of an Object, which
        changes some semantics slightly. In other words, this function exists
        purely for the benefit of the Java Autobus API.
        """
        return self(*args)
    
    def send(self, *args):
        """
        Invokes this function asynchronously. self(...) is exactly the same as
        self.send(...), except that the call is sent off to the server, None
        is returned to the caller, and the return value or exception send from
        the server, if any, is discarded.
        """
        instance_args = [encode_object(arg) for arg in args]
        message = create_message(CallFunctionCommand,
                NOTIFICATION, interface_name=self.interface.name,
                function=self.name, arguments=instance_args)
        self.connection.send(message)
    
    def invoke_later(self, args):
        """
        Same as send(*args). This exists to make libautobus from Java easier.
        """
        return self.send(*args)
    
    def invoke_later_py(self, args):
        return self.invoke_later(args)
    
    def __str__(self):
        return ("<FunctionWrapper on function " + self.name + " and interface "
                + str(self.interface) + ">")
    
    __repr__ = __str__

class ObjectWrapper(ObjectWrapperSuper):
    pass

class EventWrapper(EventWrapperSuper):
    pass

class LocalInterface(object):
    """
    A local interface. Instances of this class represent interfaces that have
    been added locally to the AutobusConnection instance. They will be
    registered with the server when a connection is established.
    """
    def __init__(self, name, doc):
        self.connection = None
        self.name = name
        self.doc = doc
        if doc is None:
            self.doc = ""
        self.functions = {} # Maps names to LocalFunction objects representing
        # functions we've registered on this interface
        self.events = {} # Maps names to LocalEvent objects representing
        # events we've registered on this interface
        self.objects = {} # Maps names to LocalObject objects representing
        # objects we've registered on this interface
    
    def add_all(self, interface):
        """
        Adds all of the functions present on the specified object that do not
        start with an underscore to this interface. This is used by
        AutobusConnection.add_interface.
        """
        for function_name in dir(interface):
            if function_name[0:1] != "_" and callable(
                    getattr(interface, function_name)):
                self.functions[function_name] = LocalFunction(function_name,
                        get_function_doc(interface, function_name),
                        getattr(interface, function_name))
    def register_function(self, local_function):
        """
        Registers the specified LocalFunction object to this interface.
        """
        self.functions[local_function.name] = local_function
        local_function.interface = self
    
    def register_event(self, local_event):
        """
        Registers the specified LocalEvent object to this interface.
        """
        self.events[local_event.name] = local_event
        local_event.interface = self
    
    def register_object(self, local_object):
        """
        Registers the specified LocalObject object to this interface.
        """
        self.objects[local_object.name] = local_object
        local_object.interface = self

class LocalFunction(object):
    """
    A local function. Local functions are stored in a LocalInterface instance's
    function map.
    """
    def __init__(self, name, doc, function):
        self.interface = None
        self.name = name
        self.doc = doc
        if doc is None:
            self.doc = ""
        if is_jython and hasattr(function, "run"):
            self.start_thread = function.isInNewThread()
            self.function = function.run
        else:
            self.start_thread = hasattr(function, "start_thread") and function.start_thread
            self.function = function

class LocalEvent(object):
    """
    A local event. Local events are callable; calling one will fire it.
    """
    def __init__(self, name, doc):
        """
        Creates a new local event. The event will use the specified name and
        docstring.
        """
        self.interface = None
        self.name = name
        self.doc = doc
    
    def __call__(self, *arguments):
        """
        Fires this event, using the specified arguments as the event's
        arguments. If no connection to the server is available, this does
        nothing.
        """
        with self.interface.connection.on_connect_lock:
            message = create_message(FireEventCommand,
                    NOTIFICATION, interface_name=self.interface.name,
                    event_name=self.name, 
                    arguments=[encode_object(o) for o in arguments])
            try:
                self.interface.connection.send(message)
            except NotConnectedException: # We've already stored the value
                # locally if we're calling notify, so we'll just rely on the
                # connect thread to send this value on next connection.
                pass

class LocalObject(object):
    """
    A local object.
    """
    def __init__(self, name, doc, value):
        """
        Creates a new local object. The object will use the specified name,
        docstring, and it will have the specified initial value.
        """
        self.interface = None
        self.name = name
        self.doc = doc
        if doc is None:
            self.doc = ""
        encode_object(value) # Make sure in advance that we can encode the
        # value the user's trying to put as the object's default value
        self.value = value
    
    def set(self, new_value):
        encode_object(new_value) # Same thing as in the constructor; make sure
        # in advance we can successfully encode the new value for the object
        self.value = new_value
        self.notify()
    
    def notify(self):
        """
        Sends a message to the server notifying it that this object's value has
        changed. The encoded form of the object will be sent in the message.
        """
        with self.interface.connection.on_connect_lock:
            message = create_message(SetObjectCommand,
                    NOTIFICATION, interface_name=self.interface.name,
                    object_name=self.name, value=encode_object(self.value))
            try:
                self.interface.connection.send(message)
            except NotConnectedException: # We've already stored the value
                # locally if we're calling notify, so we'll just rely on the
                # connect thread to send this value on next connection.
                pass

class AutobusConnection(AutobusConnectionSuper):
    """
    A connection to an Autobus server. The typical way to use libautobus is to
    create an instance of this class and go from there.
    
    Right now, all interfaces that are to be made available must be specified
    before the connection is connected for the first time. They can be
    specified after, but this class won't register them until it reconnects.
    
    If the connection to the autobus server is lost, all currently-pending
    functions etc will raise an exception, and this class will attempt to
    re-establish a connection and re-register interfaces.
    
    Subscripting an instance of this class is the same as calling
    get_interface(). For example, a wrapper around the interface "example" on
    the server could be obtained with some_autobus_connection["example"].
    
    Autobus provides introspection and additional information via the built-in
    autobus interface. You can access it with get_interface("autobus") or
    connection["autobus"]. Calling
    connection["autobus"].list_functions("autobus") will list all of the
    functions available on the autobus interface along with more documentation
    on how to use them.
    
    If reconnect is True (the default), the connection will reconnect itself
    and re-register all of its local interfaces and functions when it gets
    disconnected. It will continue attempting to reconnect indefinitely. If
    reconnect is False, it's up to the on_disconnect function (or some other
    functionality) to call the connect method when the connection is lost.
    
    If print_exceptions is True, any exceptions thrown by a local function
    invoked by a remote client will be printed with traceback.print_exc()
    before being sent back to the client that invoked the function. This is
    useful when a function is raising an unexpected error and more information
    about the error, such as its traceback, is needed. 
    """
    def __init__(self, host=None, port=None, reconnect=True,
            print_exceptions=False, on_connect=lambda: None,
            on_disconnect=lambda: None):
        """
        Creates a new connection. This doesn't actually connect to the
        autobus server; use connect() or start_connecting() for that.
        """
        if host is None:
            host = os.getenv("AUTOBUS_SERVER")
        if host is None:
            host = "localhost"
        if port is None:
            port = os.getenv("AUTOBUS_PORT")
        if port is None:
            port = DEFAULT_PORT
        if isinstance(port, basestring):
            port = int(port)
        self.socket = None
        self.host = host
        self.port = port
        self.reconnect = reconnect
        self.on_connect = on_connect
        self.on_disconnect = on_disconnect
        self.print_exceptions = print_exceptions
        self.on_connect_lock = RLock()
        self.interfaces = {} # Map of names to LocalInterface objects
        # representing interfaces we've registered
        self.is_shut_down = False
        self.send_queue = Queue()
        self.receive_queues = {} # Maps message ids expecting responses to the
        # corresponding queues waiting for the message response
        self.event_listeners = {} # Maps tuples containing an interface name
        # and an event name to a list of functions listening for the specified
        # event to fire on the server. At least one entry must be present in
        # each list in order for AutobusConnection to auto-register a
        # listener on the specified event on connect, even if it's as simple
        # as lambda: None.
        self.object_values = {} # Maps tuples containing an interface name and
        # an object name to the object's current value as sent by the server.
        # This dict gets replaced every time we disconnect.
        self.object_listeners = {} # Maps tuples containing an interface name
        # and an object name to a list of functions listening for changes in
        # that object. At least one entry must be present in each list in order
        # for AutobusConnection to auto-register a watch on the object on
        # connect, even if it's as simple as lambda: None.
    
    def shutdown(self):
        self.is_shut_down = True
        try:
            self.socket.shutdown(SHUT_RDWR)
        except:
            pass
        try:
            self.socket.close()
        except:
            pass
    
    def add_interface(self, name, interface=None):
        """
        Adds an interface that will be automatically registered with the server
        on connecting. All methods that do not start with an underscore on the
        specified object will be registered on the interface as functions. The
        specified object's docstring, if it has one, will be used as the
        interface's documentation.
        
        If the specified interface is None, no functions will be registered on
        the interface. Either way, functions can later be registered with
        add_function.
        """
        self.interfaces[name] = LocalInterface(name, inspect.getdoc(interface))
        self.interfaces[name].connection = self
        if interface is not None:
            self.interfaces[name].add_all(interface)
    
    def add_function(self, interface_name, function_name, doc, function):
        """
        Adds a function to the specified local interface. The interface must
        already have been registered with add_interface. The function will use
        the specified name and documentation (which must not be None: it should
        be the empty string if no documentation is available). When another
        client calls the specified function, the last argument to this
        function, which should be callable, will be invoked and its return
        value sent back to the invoking client. The function can raise an
        exception, which will be re-thrown on the client side.
        """
        self.interfaces[interface_name].register_function(LocalFunction(
                function_name, doc, function))
    
    def add_event_listener(self, interface_name, event_name, function):
        """
        Adds a function that will be notified when the specified remote event
        is fired. This function is not thread-safe and should generally only
        be called before the first connection to the server is created.
        
        The specified function should accept a number of arguments
        corresponding to the number of arguments sent when the remote client
        fires the event. You'll usually have to consult the event's
        documentation.
        """
        if hasattr(function, "fired"): # We're on Jython and the function is
            # an instance of EventListener
            function = function.fired
        event_spec = interface_name, event_name
        if event_spec not in self.event_listeners:
            self.event_listeners[event_spec] = []
        self.event_listeners[event_spec].append(function)
    
    def add_event(self, interface_name, event_name, doc):
        """
        Adds a local event to the specified interface. The LocalEvent
        instance created for the event will be returned.
        """
        event = LocalEvent(event_name, doc)
        self.interfaces[interface_name].register_event(event)
        return event
    
    def add_object_watch(self, interface_name, object_name, function):
        """
        Adds a function that will be notified when the specified remote
        object's value changes. add_object_watch is not thread-safe and should
        generally only be called before the first connection to the server is
        created.
        
        The function should accept one argument. This argument will be the new
        value of the object.
        
        The function must not interact with Autobus. If it does, deadlock will
        likely result, as the input thread, which receives packets from the
        server (such as responses to function calls), blocks while this
        function is invoked. If you need to interact with Autobus (for example,
        to call functions etc), the function you supply should start a new
        thread to do so.
        """
        if hasattr(function, "changed"): # We're on Jython and the function
            # is an instance of ObjectListener
            function = function.changed
        object_spec = interface_name, object_name
        if object_spec not in self.object_listeners:
            self.object_listeners[object_spec] = []
        self.object_listeners[object_spec].append(function)
    
    def add_object(self, interface_name, object_name, doc, value):
        """
        Adds a shared object to the specified interface. The LocalObject
        instance created for the object will be returned.
        """
        object = LocalObject(object_name, doc, value)
        self.interfaces[interface_name].register_object(object)
        return object
    
    def start_connecting(self):
        """
        Calls connect(None) in a new thread. Any exception thrown by the call
        to connect(None) is discarded without being printed to stdout.
        """
        def target():
            try:
                self.connect(attempts=None)
            except:
                pass
        Thread(target=target).start()
    
    def connect(self, attempts=1):
        """
        Connects to the Autobus server. The specified number of attempts will
        be made to re-establish a connection, each time waiting an amount of
        time that increments itself up to 20 seconds. If attempts is None, an
        infinite number of attempts will be made. Once a connection has been
        established, this method returns. If it fails to establish a
        connection, an exception will be raised.
        """
        if attempts == 0:
            attempts = None
        progress = 0
        delay = 0.1
        delay_increment = 1.5
        while (attempts is None or progress < attempts) and not self.is_shut_down:
            progress += 1
            delay *= delay_increment
            if delay > 20:
                delay = 20
            self.socket = Socket()
            try:
                self.socket.connect((self.host, self.port))
            except:
                sleep(delay)
                continue
            with self.on_connect_lock:
                self.connection_established()
            return
        raise Exception("Couldn't connect")
    # messagearrived, inputclosed, readnextmessage
    
    def connection_established(self):
#        print "Processing established connection..."
        self.input_thread = InputThread(self.socket, self.message_arrived, self.input_closed)
        self.output_thread = OutputThread(self.socket, self.read_next_message, self.message_write_finished)
        self.send_queue = Queue() # clear the send queue
        self.receive_queues = {}
        self.input_thread.start()
        self.output_thread.start()
#        print "Registering interfaces with the server..."
        for interface in self.interfaces.values():
            self.send(create_message(RegisterInterfaceCommand,
                    NOTIFICATION, name=interface.name, doc=interface.doc))
            message = create_message(RegisterFunctionCommand,
                    NOTIFICATION, interface_name=interface.name, name=[], doc=[])
            for function in interface.functions.values():
                message["name"].append(function.name)
                message["doc"].append(function.doc)
            self.send(message)
            for event in interface.events.values():
                self.send(create_message(
                        RegisterEventCommand, NOTIFICATION,
                        interface_name=interface.name, event_name=event.name,
                        doc=event.doc))
            for object in interface.objects.values():
                self.send(create_message(
                        RegisterObjectCommand, NOTIFICATION,
                        interface_name=interface.name, object_name=object.name,
                        doc=object.doc, value=encode_object(object.value)))
        for interface_name, event_name in self.event_listeners.keys():
            self.send(create_message(
                    RegisterListenerCommand, NOTIFICATION,
                    interface_name=interface_name, event_name=event_name))
        for interface_name, object_name in self.object_listeners.keys():
            message = create_message(
                    WatchObjectCommand, COMMAND,
                    interface_name=interface_name, object_name=object_name)
            self.send(message) # I used to send this as a query and set the
            # resulting value into the object, but this caused a race condition
            # where another value arriving as a SetObjectCommand before the
            # logic in this method could get the new value set into the object
            # would cause the object to be reset to its initial value. So for
            # now, we're processing the WatchObjectResponse in message_arrived
            # the same as we process SetObjectCommand instances from the
            # server, which fixes the race condition. That's why it's a command
            # but we're not querying for the response.
        # That's it! Now we run the user's on_connect function.
        self.on_connect()
        
    def message_arrived(self, message):
#        print "Message from the server arrived: " + str((message)) + "\n\n"
        queue = self.receive_queues.get(message["message_id"], None)
        if queue is not None:
            queue.put(message)
            try:
                del self.receive_queues[message["message_id"]]
            except KeyError:
                pass
            return
        if message["action"] == RunFunctionCommand:
            interface_name = message["interface_name"]
            function_name = message["function"]
            arguments = [decode_object(arg) for arg in message["arguments"]]
            try:
                if function_name.startswith("_"):
                    raise Exception("Function names starting with _ are "
                            "not allowed")
                interface = self.interfaces[interface_name]
                function = interface.functions[function_name]
            except Exception, e:
                if self.print_exceptions:
                    print_exc()
                self.send(create_message(RunFunctionResponse,
                        message, return_value=encode_object(e)))
            def process():
                try:
                    return_value = function.function(*arguments)
                except:
                    if self.print_exceptions:
                        print_exc()
                    _, return_value, _ = sys.exc_info() #@UndefinedVariable
                self.send(create_message(RunFunctionResponse,
                        message, return_value=encode_object(return_value)))
            if function.start_thread:
                Thread(target=process).start()
            else:
                process()
            return
        if message["action"] == FireEventCommand:
            interface_name = message["interface_name"]
            event_name = message["event_name"]
            arguments = message["arguments"]
            event_spec = (interface_name, event_name)
            self.fire_event_listeners(event_spec, [decode_object(o) for o in arguments])
            return
        if message["action"] in (SetObjectCommand, WatchObjectResponse):
            interface_name = message["interface_name"]
            object_name = message["object_name"]
            value = message["value"]
            object_spec = (interface_name, object_name)
            self.object_values[object_spec] = decode_object(value)
            self.notify_object_listeners(object_spec)
            return
        if message["action"] == PingCommand:
            self.send(create_message(PingResponse, message))
        print "Not processing message for action " + message["action"]
    
    def notify_object_listeners(self, object_spec):
        """
        This should only be used by AutobusConnection itself. Notifies all
        listeners on the specified object spec about the object's current
        value. 
        """
        value = self.object_values.get(object_spec, None)
        if object_spec in self.object_listeners:
            for listener in self.object_listeners[object_spec]:
                try:
                    listener(value)
                except:
                    print_exc()
    
    def fire_event_listeners(self, event_spec, arguments):
        """
        This should only be used by AutobusConnection itself. Notifies all
        listeners on the specified event spec that the event has been fired
        with the specified arguments.
        """
        if event_spec in self.event_listeners:
            for listener in self.event_listeners[event_spec]:
                try:
                    listener(*arguments)
                except:
                    print_exc()
    
    def query(self, message, timeout=30):
        """
        Sends the specified message from the server, waiting up to the
        specified timeout for a response. When a response is received, it is
        returned. If a response is not received within the specified amount of
        time, a TimeoutException will be raised. Otherwise, the response sent
        by the server will be returned. 
        """
        queue = Queue()
        self.receive_queues[message["message_id"]] = queue
        self.send(message)
        try:
            response = queue.get(block=True, timeout=timeout)
        except Empty:
            try:
                del self.receive_queues[message["message_id"]]
            except KeyError:
                pass
            raise TimeoutException()
        # If we don't get a KeyError, it's guaranteed that the input thread
        # will already have removed the queue, so we don't need to worry about
        # removing it.
        return response
    
    def send(self, message):
        """
        Adds the specified message, which should be an instance of
        protobuf.Message, to the send queue. If the message is None, this
        method returns without doing anything.
        """
        if message is not None:
#            print "Adding message to send queue " + str(self.send_queue) + "..."
            queue = self.send_queue
            if queue is None:
                raise NotConnectedException()
            queue.put(message, block=True)
    
    def input_closed(self):
        self.send_queue.put(None)
        try:
            while True:
                self.send_queue.task_done()
        except ValueError: # Happens once we've marked all tasks as complete
            pass
        for queue in self.receive_queues.values():
            queue.put(None)
        self.receive_queues.clear()
        self.input_thread = None
        self.output_thread = None
        self.send_queue = None
        self.receive_queues = None
        self.object_values.clear()
        self.object_values = {}
        for object_spec in self.object_listeners.keys():
            self.notify_object_listeners(object_spec)
        self.on_disconnect()
        if self.reconnect and not self.is_shut_down:
            self.start_connecting()
    
    def read_next_message(self):
        send_queue = self.send_queue
        if send_queue is None:
            return None
#        print "Getting next message from send queue " + str(send_queue) + "..."
        return send_queue.get(block=True)
    
    def message_write_finished(self):
        send_queue = self.send_queue
        if send_queue is not None:
            try:
                send_queue.task_done()
            except:
                pass
    
    def send_error(self, in_reply_to=None, **kwargs):
        """
        Sends an error back to the client. If in_reply_to is itself a
        notification message, no message will be sent back. Otherwise, an
        ErrorResponse to the specified message will be sent. The error's
        fields will be initialized to have any additional keyword arugments
        specified. You'll typically do:
        
        send_error(some_message, text="A random problem happened")
        """
        if in_reply_to == None:
            in_reply_to = NOTIFICATION
        message = create_message(ErrorResponse, in_reply_to, **kwargs)
        self.send(message)
    
    def get_interface(self, name):
        """
        Returns a wrapper around the specified interface. This doesn't check
        to make sure that the specified interface is actually present on the
        server; if it's not, an exception will be thrown when a function on
        the interface is invoked for the first time.
        
        This function can be used like so:
        
        interface = some_connection.get_interface("example")
        return_value = interface.some_function("first", 2, ["third"])
        
        This would invoke the function "some_function" on the interface
        "example" on the server. An exception would be thrown if the specified
        interface does not exist on the server or if the specified interface
        does not have any such function.
        
        If the function does not return within 30 seconds (or the server does
        not send a response indicating that the function has returned within
        that time frame), a TimeoutException will be raised.
        
        This function returns a wrapper around a server-side interface. To get
        access to a local interface to register functions, events, or objects,
        use get_local_interface.
        """
        return InterfaceWrapper(self, name)
    
    __getitem__ = get_interface
    
    def get_local_event(self, interface_name, event_name):
        return self.interfaces[interface_name].events[event_name]
    
    def get_local_object(self, interface_name, object_name):
        """
        Returns the LocalObject instance representing the specified local
        object, which must have been previously registered with this
        connection. This is short for
        get_local_interface(interface_name).objects[object_name].
        """
        return self.interfaces[interface_name].objects[object_name]
    
    def get_local_function(self, interface_name, function_name):
        """
        Returns the LocalFunction instance representing the specified local
        function, which must have been previously registered with this
        connection. This is short for
        get_local_interface(interface_name).functions[function_name]
        """
        return self.interfaces[interface_name].functions[function_name]
    
    def get_local_interface(self, interface_name):
        """
        Returns the LocalInterface instance representing the specified local
        interface.
        """
        return self.interfaces[interface_name]
    
    def set_local_object(self, interface_name, object_name, value):
        """
        Sets the value of the specified local object and notifies the server if
        there's currently an active connection to the server. This is short for
        get_local_object(interface_name, object_name).set(value).
        """
        self.get_local_object(interface_name, object_name).set(value)
    
    def get_object_value(self, interface_name, object_name):
        """
        Returns the current value of a remote object, or None if the value is
        not known (which could be because add_object_watch was not called before
        the last time this connection successfully connected to the server).
        """
        return self.object_values.get((interface_name, object_name), None)
    
    def flush(self):
        """
        If this connection is currently connected, waits until all pending
        outbound messages have been successfully sent to the server.
        
        WARNING: I haven't gone through this with a fine-toothed comb for race
        conditions, so there very well could be some that could cause this
        function to deadlock. Use at your own risk, and thoroughly test any
        application using this (in particular, test out what happens when you
        kill the Autobus server around the time you call this method).
        """
        send_queue = self.send_queue
        if send_queue is not None:
            send_queue.join()
    
    def unshutdown(self):
        """
        Marks a shutdown Autobus connection as no longer shut down. This
        generally should not be used in production code; it's mostly for those
        that want to disconnect and reconnect an Autobus connection
        experimentally. The connection must have previously been shut down with
        shutdown() and sufficient time given for the connection to stop trying
        to reconnect. After this is called, all local interfaces, functions,
        objects, etc, will still be registered to this connection, and a call
        to connect() or start_connecting() immediately after calling this
        function will cause them to be re-registered with the server.
        """
        self.is_shut_down = False
    
















