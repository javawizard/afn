


from threading import Thread, RLock
from Queue import Queue
from libautobus import ProtoMultiAccessor, read_fully, discard_args
from libautobus import InputThread, OutputThread
from struct import pack, unpack
import autobus_protobuf.autobus_pb2 as protobuf
from traceback import print_exc
from socket import socket as Socket
from functools import partial
import re
import sys

COMMAND = 1
RESPONSE = 2
NOTIFICATION = 3

connection_map = {} # connection ids to connection objects
interfaces_by_name = {} # interface names to lists of interfaces
interface_map = {} # interface ids to interfaces
event_queue = Queue() # A queue of tuples, each of which consists of a
# connection id and either a protobuf Message object or None to indicate that
# the connecton was lost and so should be shut down

next_id = 1
next_id_lock = RLock()

# We're breaking naming conventions with these two because I'm paranoid of
# accidentally conflicting them if I use the regular naming convention
MessageValue = ProtoMultiAccessor(protobuf.Message, "value")
InstanceValue = ProtoMultiAccessor(protobuf.Instance, "value")

def get_next_id():
    """
    Thread-safely increments next_id and returns the value it previously held.
    """
    with next_id_lock:
        the_id = next_id
        next_id += 1
    return the_id

def process_event_queue():
    while True:
        sender, message = event_queue.get(block=True)
        if sender is None: # pushed on at KeyboardInterrupt to shut down the event queue
            break
        connection = connection_map.get(sender, None)
        try:
            message(sender, connection)
        except:
            print "FATAL ERROR in a protocol function handler:"
            print_exc()
            break
    for connection in connection_map.values():
        connection.shutdown_and_deregister()
    

class Interface(object):
    """
    Represents an interface registered by a particular connection. Interfaces
    have an id, a name, and a reference to the connection that registered the
    interface. They also have a dict mapping names of functions to the
    corresponding function objects, and they have a single Instance
    representing more information about the interface.
    """
    def __init__(self, name, connection, info):
        """
        Creates a new interface object. This does not register it with all of
        the global dicts nor the connection's dicts; register() must be called
        to do that.
        """
        self.id = get_next_id()
        self.name = name
        self.connection = connection
        self.function_map = {}
        self.info = info
    
    def register(self):
        """
        Registers this interface with the global dicts and the connection's
        dicts. This must only be called on the event thread.
        """
        if not self.name in interfaces_by_name:
            interfaces_by_name[self.name] = []
        interfaces_by_name[self.name].append(self)
        interface_map[self.id] = self
        self.connection.interfaces.append(self)
    
    def deregister(self):
        """
        The opposite of register(): deregisters this interface.
        """
        if self.name in interfaces_by_name:
            interfaces_by_name[self.name].remove(self)
            if interfaces_by_name[self.name] == []:
                del interfaces_by_name[self.name]
        del interface_map[self.id]
        self.connection.interfaces.remove(self)

class Connection(object):
    """
    Represents a connection. Connections are created when someone connects to
    autobus. They are destroyed when autobus exits or the connection is closed.
    Connections have an id, a list of interfaces they have registered, a queue
    containing protobuf Message objects to be sent to the corresponding
    socket as soon as possible, and an input and output thread.
    """
    def __init__(self, socket):
        self.socket = socket
        self.interfaces = []
        self.message_queue = Queue()
        self.id = get_next_id()
        self.blocked_messages = set()
        self.input_thread = InputThread(socket, self.message_arrived,
                                        self.input_closed)
        self.output_thread = OutputThread(socket, self.read_next_message)
    
    def register(self):
        """
        Registers this connection with the global connection map. This must
        only be called on the event thread.
        """
        connection_map[self.id] = self
    
    def message_arrived(self, message):
        try:
            function = find_function_for_message(message)
        except:
            print "Client sent an invalid command (they will be disconnected):"
            print_exc()
            raise
        event_queue.put((self.id, partial(function, message)), block=True)
    
    def input_closed(self):
        event_queue.put((self.id, discard_args(self.shutdown_and_deregister)),
                        block=True)
        self.message_queue.put(None, block=True)
    
    def read_next_message(self):
        return self.message_queue.get(block=True)
    
    def close(self):
        """
        Closes the underlying socket for this connection. This, in turn, causes
        the input and output threads to shut down. This can be called from any
        thread.
        """
        self.socket.close()
    
    def send(self, message):
        """
        Adds the specified message to this connection's message queue. It will
        be sent to the underlying socket as soon as possible by the
        connection's output thread. This can be called from any thread.
        """
        self.message_queue.put(message, block=True)
    
    def start(self):
        """
        Starts this connection's input and output threads.
        """
        self.input_thread.start()
        self.output_thread.start()
    
    def shutdown_and_deregister(self):
        """
        Shuts this connection down and deregisters it. This also pushes error
        messages onto the message queues of any connections that were waiting
        for a response from this connection. This must only be called from the
        event thread.
        """
        self.socket.close()
        if self.id in connection_map:
            del connection_map[self.id]
        for connection_id, message_id in self.blocked_messages:
            if connection_id in connection_map:
                connection_map[connection_id].send(create_error_response(
                        message_id, "The connection this response was waiting "
                        "on closed unexpectedly"))
        for interface in self.interfaces:
            interface.deregister()
        

def create_error_response(message_id, text):
    response = protobuf.ErrorResponse()
    response.text = text
    message = protobuf.Message()
    message.message_type = RESPONSE
    message.message_id = message_id
    MessageValue[message] = response
    return message

def find_function_for_message(message):
    class_name = type(message).__name__
    function_name = re.sub("([A-Z])", "_\\1", class_name)[1:].lower()
    function_name = "process_" + function_name
    if function_name not in globals():
        raise Exception("Message is missing processor function "
                        + function_name)
    return globals()[function_name]

def create_message_pair(type_or_instance):
    if callable(type_or_instance):
        type_or_instance = type_or_instance()
    message = protobuf.Message()
    MessageValue[message] = type_or_instance
    return message, type_or_instance

def process_register_interface_command(message, sender, connection):
    command = MessageValue[message]
    name = command.name
    info = command.info
    interface = Interface(name, connection, info)
    interface.register()
    response_message, response = create_message_pair(protobuf.RegisterInterfaceResponse)
    response.id = interface.id
    return response_message


# MAIN CODE

server = Socket()
if len(sys.argv) > 1:
    server_port = int(sys.argv[1])
else:
    server_port = 28862
server.bind(("", server_port))
server.listen(50)

try:
    while True:
        socket = server.accept()
        connection = Connection(socket)
        event_queue.put((connection.id, discard_args(connection.register)), block=True)
        connection.start()
except KeyboardInterrupt:
    print "KeyboardInterrupt received, shutting down"
    print_exc()
    event_queue.put((None, None), block=True)
    print "Event queue has been notified to shut down"
except:
    print "Unexpected exception occurred in the main loop, shutting down"
    print_exc()
    event_queue.put((None, None), block=True)
    print "Event queue has been notified to shut down"

    




        



















