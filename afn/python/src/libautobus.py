
from threading import Thread, RLock
from struct import pack, unpack
import autobus_protobuf.autobus_pb2 as protobuf
from traceback import print_exc
from socket import error as SocketError, socket as Socket, SHUT_RDWR
from functools import update_wrapper
from Queue import Queue, Empty
from time import sleep
import inspect

COMMAND = 1
RESPONSE = 2
NOTIFICATION = 3

DEFAULT_PORT = 28862

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

class ProtoMultiAccessor(object):
    def __init__(self, proto_class, prefix, specifier=None):
        self.proto_class = proto_class
        self.prefix = prefix
        if specifier is None:
            specifier = prefix + "_n"
        self.specifier = specifier
        self.fields = {}
        for field_name, field_descriptor in proto_class.DESCRIPTOR.fields_by_name.items():
            if field_name.startswith(prefix) and field_name != specifier:
                self.fields[field_descriptor.message_type._concrete_class] = field_name
    
    def __getitem__(self, item):
        return getattr(item, self.prefix + getattr(item, self.specifier))
    
    def __setitem__(self, item, value):
        set = False
        for field_class, field_name in self.fields.items():
            getattr(item, field_name).Clear()
            if isinstance(value, field_class):
                set = True
                getattr(item, field_name).MergeFrom(value)
                setattr(item, self.specifier, field_name[len(self.prefix):])
        if not set:
            raise Exception("Supplied instance is not of a valid type")

MessageValue = ProtoMultiAccessor(protobuf.Message, "value")
InstanceValue = ProtoMultiAccessor(protobuf.Instance, "value")

def create_message_pair(*args, **kwargs):
    """
    Creates a message object and optionally an instance of a particular message
    type and binds them together. This completely initializes the generic
    message instance; only the instance of the specific type of message needs
    to be modified. The first argument passed to this method is either the type
    of the particular message type to use or an instance of such a type. The
    second, optional, argument is a message that this message should be
    initialized to be a reply to or one of the three message type globals to
    specify the message's type and initialize it with a default message id.
    
    Any additional keyword arguments can be used to pre-initialize fields on
    the specific message type.
    
    This function returns the constructed message followed by the constructed
    specific message (or the first argument if it was a specific message
    instance instead of a specific message type).
    
    The constructed message returned will be None if this method is called to
    construct a reply to a notification (since, by protocol definition,
    only commands, and possibly replies, are replied to; notifications are
    never replied to). The specific message will still be present, though. This
    allows code to call this function, set up the specific message, and send
    the constructed message without worrying about whether or not it's None,
    and the message sending logic will discard it if it's None.
    """
    # type_or_instance, in_reply_to, notification
    type_or_instance = args[0]
    message_type = COMMAND if len(args) < 2 else args[1]
    if callable(type_or_instance):
        type_or_instance = type_or_instance(**kwargs)
    message = protobuf.Message()
    MessageValue[message] = type_or_instance
    if isinstance(message_type, int):
        message.message_id = get_next_id()
        message.message_type = message_type
    else:
        if message_type.message_type == NOTIFICATION:
            # Replies to notifications are not allowed
            message = None
        else:
            message.message_id = message_type.message_id
            message.message_type = RESPONSE
    return message, type_or_instance

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
        new_data = socket.recv(length - len(data))
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
                message = protobuf.Message()
                message.ParseFromString(message_data)
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
                    message_data = message.SerializeToString()
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

def encode_object(object, instance=None):
    """
    Encodes an object to an instance of protobuf.Instance. This does not yet
    support struct instances or timestamp instances.
    """
    if instance is None:
        instance = protobuf.Instance()
    if isinstance(object, int):
        InstanceValue[instance] = protobuf.IntegerInstance(value=object)
    elif isinstance(object, long):
        InstanceValue[instance] = protobuf.LongInstance(value=object)
    elif isinstance(object, float):
        InstanceValue[instance] = protobuf.DoubleInstance(value=object)
    elif isinstance(object, bool):
        InstanceValue[instance] = protobuf.BoolInstance(value=object)
    elif isinstance(object, basestring):
        InstanceValue[instance] = protobuf.StringInstance(value=object)
    elif object is None:
        InstanceValue[instance] = protobuf.NullInstance()
    elif isinstance(object, (list, tuple)):
        list_instance = protobuf.ListInstance()
        for list_object in object:
            list_object_instance = list_instance.value.add()
            encode_object(list_object, list_object_instance)
        InstanceValue[instance] = list_instance
    elif isinstance(object, dict):
        map_instance = protobuf.MapInstance()
        for key, value in object.items():
            map_entry = map_instance.value.add()
            map_entry.key.MergeFrom(encode_object(key))
            map_entry.value.MergeFrom(encode_object(value))
        InstanceValue[instance] = map_instance
    elif isinstance(object, Exception):
        InstanceValue[instance] = protobuf.ExceptionInstance(
                text=type(object).__name__ + ": " + str(object))
    else:
        raise Exception("Invalid instance type to encode: " + 
                str(type(object)))
    return instance

def decode_object(instance):
    instance_value = InstanceValue[instance]
    if isinstance(instance_value, (protobuf.IntegerInstance,
            protobuf.LongInstance, protobuf.DoubleInstance,
            protobuf.StringInstance)):
        return instance_value.value
    elif isinstance(instance_value, protobuf.BoolInstance):
        return bool(instance_value.value)
    elif isinstance(instance_value, protobuf.NullInstance):
        return None
    elif isinstance(instance_value, protobuf.ListInstance):
        result = []
        for item in instance_value.value:
            result.append(decode_object(item))
        return result
    elif isinstance(instance_value, protobuf.MapInstance):
        result = {}
        for item in instance_value.value:
            result[decode_object(item.key)] = decode_object(item.value)
        return result
    elif isinstance(instance_value, protobuf.ExceptionInstance):
        return Exception(instance_value.text)
    else:
        raise Exception("Invalid instance type to decode: " + 
                str(type(instance_value)))

def get_function_doc(interface, function_name):
    function = getattr(interface, function_name)
    if inspect.isfunction(function):
        args = inspect.formatargspec(*inspect.getargspec(function))
    elif inspect.ismethod(function):
        argspec = inspect.getargspec(function)
        # Remove the leading "self" argument
        argspec = (argspec[0][1:],) + argspec[1:]
        args = inspect.formatargspec(*argspec)
    else:
        args = "(...)"
    doc = inspect.getdoc(function)
    if doc is None:
        doc = ""
    return function_name + args + "\n\n" + doc

class InterfaceWrapper(object):
    def __init__(self, connection, name):
        self.connection = connection
        self.name = name
    
    def __getattr__(self, attribute):
        return FunctionWrapper(self.connection, self, attribute)

class FunctionWrapper(object):
    def __init__(self, connection, interface, name):
        self.connection = connection
        self.interface = interface
        self.name = name
    
    def __call__(self, *args):
        instance_args = [encode_object(arg) for arg in args]
        message, call_message = create_message_pair(protobuf.CallFunctionCommand,
                interface_name=self.interface.name, function=self.name,
                arguments=instance_args)
        response = self.connection.query(message)
        call_response = MessageValue[response]
        if isinstance(call_response, protobuf.ErrorResponse):
            raise Exception("Server-side error: " + call_response.text)
        result = decode_object(call_response.return_value)
        if isinstance(result, Exception):
            raise result
        return result

class ObjectWrapper(object):
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
        self.function = function

class LocalObject(object):
    """
    A local object.
    """
    def __init__(self, name, doc, value):
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
            message, set_message = create_message_pair(protobuf.SetObjectCommand,
                    NOTIFICATION, interface_name=self.interface.name,
                    object_name=self.name, value=encode_object(self.value))
            self.interface.connection.send(message)

class AutobusConnection(object):
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
    """
    def __init__(self, host="localhost", port=DEFAULT_PORT, reconnect=True,
            on_connect=lambda: None, on_disconnect=lambda: None):
        """
        Creates a new connection. This doesn't actually connect to the
        autobus server; use connect() or start_connecting() for that.
        """
        self.host = host
        self.port = port
        self.reconnect = reconnect
        self.on_connect = on_connect
        self.on_disconnect = on_disconnect
        self.on_connect_lock = RLock()
        self.interfaces = {} # Map of names to LocalInterface objects
        # representing interfaces we've registered
        self.is_shut_down = False
        self.send_queue = Queue()
        self.receive_queues = {} # Maps message ids expecting responses to the
        # corresponding queues waiting for the message response
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
    
    def add_object_watch(self, interface_name, object_name, function):
        """
        Adds a function that will be notified when the specified remote
        object's value changes. add_object_watch is not thread-safe and should
        generally only be called before the first connection to the server is
        created.
        
        The function should accept one argument. This argument will be the new
        value of the object.
        """
        object_spec = interface_name, object_name
        if object_spec not in self.object_listeners:
            self.object_listeners[object_spec] = []
        self.object_listeners[object_spec].append(function)
    
    def add_object(self, interface_name, object_name, doc, value):
        """
        Adds a shared object to the specified interface.
        """
        self.interfaces[interface_name].register_object(LocalObject(
                object_name, doc, value))
    
    def start_connecting(self):
        """
        Calls connect(None) in a new thread.
        """
        Thread(target=self.connect, kwargs={"attempts": None}).start()
    
    def connect(self, attempts=1):
        """
        Connects to the Autobus server. The specified number of attempts will
        be made to re-establish a connection, each time waiting an amount of
        time that increments itself up to 20 seconds. If attempts is None, an
        infinite number of attempts will be made. Once a connection has been
        established, this method returns. If it fails to establish a
        connection, an exception will be raised.
        """
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
        self.output_thread = OutputThread(self.socket, self.read_next_message)
        self.send_queue = Queue() # clear the send queue
        self.receive_queues = {}
        self.input_thread.start()
        self.output_thread.start()
#        print "Registering interfaces with the server..."
        for interface in self.interfaces.values():
            message, register_message = create_message_pair(protobuf.RegisterInterfaceCommand,
                    NOTIFICATION, name=interface.name, doc=interface.doc)
            self.send(message)
            for function in interface.functions.values():
                message, register_message = create_message_pair(
                        protobuf.RegisterFunctionCommand, NOTIFICATION,
                        interface_name=interface.name, name=function.name,
                        doc=function.doc)
                self.send(message)
            for object in interface.objects.values():
                message, register_message = create_message_pair(
                        protobuf.RegisterObjectCommand, NOTIFICATION,
                        interface_name=interface.name, object_name=object.name,
                        doc=object.doc, value=encode_object(object.value))
                self.send(message)
        for interface_name, object_name in self.object_listeners.keys():
            message, register_message = create_message_pair(
                    protobuf.WatchObjectCommand, COMMAND,
                    interface_name=interface_name, object_name=object_name)
            response = self.query(message)
            self.object_values[(interface_name, object_name)] = (
                    decode_object(MessageValue[response].value))
            self.notify_object_listeners((interface_name, object_name)) 
#        print "Calling custom on_connection action"
        self.on_connect()
        
    def message_arrived(self, message):
        queue = self.receive_queues.get(message.message_id, None)
        if queue is not None:
            queue.put(message)
            try:
                del self.receive_queues[message.message_id]
            except KeyError:
                pass
            return
        message_value = MessageValue[message]
        if isinstance(message_value, protobuf.RunFunctionCommand):
            interface_name = message_value.interface_name
            function_name = message_value.function
            arguments = message_value.arguments
            arguments = [decode_object(arg) for arg in arguments]
            try:
                if function_name.startswith("_"):
                    raise Exception("Function names starting with _ are "
                            "not allowed")
                interface = self.interfaces[interface_name]
                function = interface.functions[function_name]
                return_value = function.function(*arguments)
            except Exception as e:
                return_value = e
            response, run_response = create_message_pair(protobuf.RunFunctionResponse,
                    message, return_value=encode_object(return_value))
            self.send(response)
            return
        if isinstance(message_value, protobuf.SetObjectCommand):
            interface_name = message_value.interface_name
            object_name = message_value.object_name
            value = message_value.value
            object_spec = (interface_name, object_name)
            self.object_values[object_spec] = decode_object(value)
            self.notify_object_listeners(object_spec)
            return
        print "Message from the server arrived: " + repr(message)
    
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
    
    def query(self, message, timeout=30):
        """
        Sends the specified message from the server, waiting up to the
        specified timeout for a response. When a response is received, it is
        returned. If a response is not received within the specified amount of
        time, a TimeoutException will be raised. Otherwise, the response sent
        by the server will be returned. 
        """
        queue = Queue()
        self.receive_queues[message.message_id] = queue
        self.send(message)
        try:
            response = queue.get(block=True, timeout=timeout)
        except Empty:
            try:
                del self.receive_queues[message.message_id]
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
            self.send_queue.put(message, block=True)
    
    def input_closed(self):
        self.send_queue.put(None)
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
            send_queue.task_done()
    
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
        message, error_message = create_message_pair(protobuf.ErrorResponse, in_reply_to, **kwargs)
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
    
















