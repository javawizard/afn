
from threading import Thread, RLock
from struct import pack, unpack
import autobus_protobuf.autobus_pb2 as protobuf
from traceback import print_exc
from socket import error as SocketError, socket as Socket
from functools import update_wrapper
from Queue import Queue

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
    with next_id_lock:
        the_id = next_id
        next_id += 1
    return the_id

class ProtoMultiAccessor(object):
    def __init__(self, proto_class, prefix):
        self.proto_class = proto_class
        self.prefix = prefix
        self.fields = {}
        for field_name, field_descriptor in proto_class.DESCRIPTOR.fields_by_name.items():
            if field_name.startswith(prefix):
                self.fields[field_descriptor.message_type._concrete_class] = field_name
    
    def __getitem__(self, item):
        for field_class, field_name in self.fields.items():
            if item.HasField(field_name):
                return getattr(item, field_name)
        raise Exception("Instance does not have any fields set")
    
    def __setitem__(self, item, value):
        set = False
        for field_class, field_name in self.fields.items():
            item.ClearField(field_name)
            if isinstance(value, field_class):
                set = True
                setattr(item, field_name, value)
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
    """
    # type_or_instance, in_reply_to, notification
    type_or_instance = args[0]
    message_type = COMMAND if len(args) < 2 else args[1]
    if callable(type_or_instance):
        type_or_instance = type_or_instance()
    message = protobuf.Message()
    MessageValue[message] = type_or_instance
    if not isinstance(message_type, int):
        message.message_id = message_type.message_id
        message.message_type = RESPONSE
    else:
        message.message_id = get_next_id()
        message.message_type = message_type
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
        new_data = socket.recv(length - len(data))
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
        self.socket = socket
        self.message_function = message_function
        self.close_function = close_function
    
    def run(self):
        try:
            while True:
                message_length = read_fully(self.socket, 4)
                message_data = read_fully(self.socket,
                                          unpack(">i", message_length)[0])
                message = protobuf.Message
                message.ParseFromString(message_data)
                self.message_function(message)
        except EOFError:
            pass
        except:
            print_exc()
        self.socket.close()
        if self.close_function:
            self.close_function()


class OutputThread(Thread):
    """
    A thread that repeatedly calls a function which should return a message
    object to send. The function can (and generally should) block as needed.
    This thread then writes the message to the socket. When the socket dies,
    it will be closed. If the function returns None, the socket will be
    immediately closed and this thread will exit.
    """
    def __init__(self, socket, read_function):
        self.socket = socket
        self.read_function = read_function
    
    def run(self):
        try:
            while True:
                message = self.read_function()
                if message is None:
                    break
                message_data = message.SerializeToString()
                self.socket.sendall(pack(">i", len(message_data)))
                self.socket.sendall(message_data)
        except SocketError:
            pass
        except:
            print_exc()
        self.socket.close()

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
    else:
        raise Exception("Invalid instance type to encode: " + 
                str(type(object)))

def decode_object(instance):
    instance_value = InstanceValue[instance]
    if isinstance(instance_value, (protobuf.IntegerInstance,
            protobuf.LongInstance, protobuf.DoubleInstance,
            protobuf.StringInstance)):
        return instance_value.value
    elif isinstance(instance_value, protobuf.NullInstance):
        return None
    elif isinstance(instance_value, protobuf.ListInstance):
        result = []
        for item in instance_value.value:
            result.append(decode_object(item))
        return result
    else:
        raise Exception("Invalid instance type to decode: " + 
                str(type(instance_value)))

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
    """
    def __init__(self, host="localhost", port=DEFAULT_PORT, 
            on_connect=lambda: None, on_disconnect=lambda: None):
        """
        Creates a new conneection. This doesn't actually connect to the
        autobus server; use connect() or start_connecting() for that.
        """
        self.host = host
        self.port = port
        self.on_connect = on_connect
        self.on_disconnect = on_disconnect
        self.source_interfaces = {}
        self.send_queue = Queue()
        self.receive_queues = {} # maps message ids expecting responses to the
        # corresponding queues waiting for the message response
    
    def add_interface(self, name, interface, info):
        """
        Adds an interface that will be automatically registered with the server
        on connecting. All methods that do not start with an underscore on the
        specified object will be registered on the interface as functions.
        """
        self.source_interfaces[name] = name, interface, info
    
    def start_connecting(self):
        """
        Calls connect(None) in a new thread.
        """
        Thread(target=self.connect, kwargs={"attempts": 1}).start()
    
    def connect(self, attempts=1):
        """
        Connects to the autobus server. The specified number of attempts will
        be made to re-establish a connection, each time waiting an amount of
        time that increments itself up to 15 seconds. If attempts is None, an
        infinite number of attempts will be made. Once a connection has been
        established, this method returns. If it fails to establish a
        connection, an exception will be raised.
        """
        progress = 0
        delay = 0.1
        delay_increment = 1.5
        while attempts is None or progress < attempts:
            progress += 1
            delay *= delay_increment
            if delay > 15:
                delay = 15
            self.socket = Socket()
            try:
                self.socket.connect((self.host, self.port))
            except:
                continue
            self.connection_established()
            return
        raise Exception("Couldn't connect")
    # messagearrived, inputclosed, readnextmessage
    
    def connection_established(self):
        self.input_thread = InputThread(self.socket, self.message_arrived, self.input_closed)
        self.output_thread = OutputThread(self.socket, self.read_next_message)
        self.send_queue = Queue() # clear the send queue
        self.input_thread.start()
        self.output_thread.start()
        for name, interface, info in self.source_interfaces.values():
            message, register_message = create_message_pair(protobuf.RegisterInterfaceCommand,
                    name=name, info=encode_object(info))
        self.on_connect()
        
    def message_arrived(self, message):
        pass
    
    def send(self, message):
        """
        Adds the specified message, which should be an instance of
        protobuf.Message, to the send queue.
        """
        self.send_queue.put(message, block=True)
    
    def input_closed(self):
        self.send_queue.put(None)
        for queue in self.receive_queues.values():
            queue.put(None)
        self.receive_queues.clear()
        self.on_disconnect()
        self.start_connecting()
    
    def read_next_message(self):
        send_queue = self.send_queue
        if send_queue is None:
            return None
        return send_queue.get(block=True)

















