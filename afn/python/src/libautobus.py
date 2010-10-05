
from threading import Thread
from struct import pack, unpack
import autobus_protobuf.autobus_pb2 as protobuf
from traceback import print_exc
from socket import error as SocketError
from functools import update_wrapper

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
                


















