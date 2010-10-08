


from threading import Thread, RLock
from Queue import Queue
from libautobus import ProtoMultiAccessor, read_fully, discard_args
from libautobus import InputThread, OutputThread, get_next_id
from libautobus import COMMAND, RESPONSE, NOTIFICATION, create_message_pair
from libautobus import DEFAULT_PORT, NoSuchInterfaceException
from libautobus import NoSuchFunctionException, encode_object, decode_object
from struct import pack, unpack
import autobus_protobuf.autobus_pb2 as protobuf
from traceback import print_exc
from socket import socket as Socket, SHUT_RDWR
from functools import partial
import re
import sys

print """\
Autobus, a message bus that's kind of a cross of D-Bus and RPC, written
by Alexander Boyd (a.k.a. javawizard or jcp)
"""

connection_map = {} # connection ids to connection objects
interfaces_by_name = {} # interface names to lists of interfaces
interface_map = {} # interface ids to interfaces
event_queue = Queue() # A queue of tuples, each of which consists of a
# connection id and either a protobuf Message object or None to indicate that
# the connecton was lost and so should be shut down

# We're breaking naming conventions with these two because I'm paranoid of
# accidentally conflicting them if I use the regular naming convention
MessageValue = ProtoMultiAccessor(protobuf.Message, "value")
InstanceValue = ProtoMultiAccessor(protobuf.Instance, "value")

def process_event_queue():
    while True:
#        print "Waiting for events..."
        sender, message = event_queue.get(block=True)
        if sender is None: # pushed on at KeyboardInterrupt to shut down the event queue
#            print "Shutdown event received"
            break
        connection = connection_map.get(sender, None)
#        print ("Event " + str(message) + " received for sender " + str(sender)
#                + " and connection " + str(connection))
        try:
            message(sender, connection)
        except:
            print "FATAL ERROR in a protocol function handler:"
            print_exc()
            break
    print "Shutting down connections..."
    connection_count = len(connection_map)
    for connection in connection_map.values():
        connection.shutdown_and_deregister()
    print str(connection_count) + " connection" + ("s" if connection_count
            != 1 else "") + " have been shut down."

class AutobusInterface(object):
    def list_interfaces(self):
        result = []
        for interface in interface_map.values():
            result.append({"id": interface.id, "name": interface.name,
                    "info": decode_object(interface.info) if interface.info
                    is not None else None, "special": interface.special})
        return result
    
    def list_functions(self, interface_id_or_name):
        result = []
        try:
            if isinstance(interface_id_or_name, basestring):
                interface = interfaces_by_name[interface_id_or_name][0]
            else:
                interface = interface_map[interface_id_or_name]
        except KeyError:
            raise Exception("No such interface.")
        for function in interface.function_map.values():
            result.append({"id": function.id, "name": function.name,
                    "special": function.special})

autobus_interface = AutobusInterface()

class Function(object):
    """
    Represents a function registered on an interface.
    """
    def __init__(self, sender, name, special_target=None):
        self.id = get_next_id()
        self.name = name
        self.sender = sender
        self.special_target = special_target
        self.special = special_target != None
    
    def invoke_special(self, message, command, connection):
        args = command.arguments
        args = [decode_object(arg) for arg in args]
        try:
            return_value = self.special_target(*args)
        except Exception as e:
            print_exc()
            return_value = e
        result = encode_object(return_value)
        response, call_response = create_message_pair(protobuf.CallFunctionResponse,
                message, return_value=result)
        connection.send(response)

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
        self.functions_by_name = {}
        self.function_map = {}
        self.info = info
        self.special = False
    
    def register(self):
        """
        Registers this interface with the global dicts and the connection's
        dicts. This must only be called on the event thread.
        """
        if not self.name in interfaces_by_name:
            interfaces_by_name[self.name] = []
        interfaces_by_name[self.name].append(self)
        interface_map[self.id] = self
        if not self.special:
            self.connection.interfaces.append(self)
    
    def register_special(self):
        """
        Registers this interface as a special interface. This sets the special
        attribute to True and completes registration normally. Special
        interfaces are those registered by the server (or any other server-side
        functionality). Their connection is, instead of being a connection, an
        object with functions directly on it.
        """
        self.special = True
        self.register()
    
    def deregister(self):
        """
        The opposite of register(): deregisters this interface.
        """
        print "Deregistering interface with name " + self.name
        if self.name in interfaces_by_name:
            interfaces_by_name[self.name].remove(self)
            if interfaces_by_name[self.name] == []:
                del interfaces_by_name[self.name]
        del interface_map[self.id]
        self.connection.interfaces.remove(self)
    
    def register_function(self, sender, function_name):
        print ("Registering function " + function_name + " from " + 
                str(sender) + " on " + self.name + "(" + str(self.id) + ")") 
        function = Function(sender, function_name)
        self.functions_by_name[function_name] = function
        self.function_map[function.id] = function
    
    def lookup_function(self, function_id=0, function_name=""):
        """
        Looks up the function with the specified id or name. If this interface
        is special, this function requires the name to be specified, and a
        special function object wrapping the function in question will be
        returned.
        """
        if self.special:
            try:
                return Function(None, function_name, getattr(self.connection, function_name))
            except AttributeError:
                raise NoSuchFunctionException(function_id, function_name)
        if function_id and function_id in self.function_map:
            return self.function_map[function_id]
        if function_name and function_name in self.functions_by_name:
            return self.functions_by_name[function_name]
        raise NoSuchFunctionException(function_id, function_name)

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
        self.pending_responses = {} # Maps message ids of commands sent to this
        # connection to tuples consisting of the id of a connection that the
        # response should be forwarded to and the message id that the response
        # should have
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
        
        If the message is None, this function returns without doing anything.
        This allows for code that constructs a message pair with
        create_message_pair and then sends the message without worrying about
        whether or not create_message_pair returned an empty message (which it
        would if it was used to "reply" to a notification instead of to a
        command).
        """
        if message is not None:
            self.message_queue.put(message, block=True)
    
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
        try:
            self.socket.shutdown(SHUT_RDWR)
        except:
            pass
        self.socket.close()
        if self.id in connection_map:
            del connection_map[self.id]
        for connection_id, message_id in self.pending_responses.values():
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
    class_name = type(MessageValue[message]).__name__
    function_name = re.sub("([A-Z])", "_\\1", class_name)[1:].lower()
    function_name = "process_" + function_name
    if function_name not in globals():
        raise Exception("Message is missing processor function "
                        + function_name)
    return globals()[function_name]

def lookup_interface(interface_id=0, interface_name="", owner=None):
    """
    Looks up an interface with the specified id or name. An interface with the
    specified id will be looked up first. If it does not exist, an interface
    with the specified name will be searched for, and the first one to be
    registered will be the one returned from this function. Either interface_id
    or interface_name can be 0 or None, respectively, which skips searching by
    that particular field. If an interface with the specified id or the
    specified name does not exist, a NoSuchInterfaceException will be raised.
    
    If owner is not None, it should be the id of a connection. Only an
    interface registered by this connection will be returned.
    """
    interface = None
    if interface_id and interface_id in interface_map:
        interface = interface_map[interface_id]
        if owner is not None and (interface.special or 
                interface.connection.id != owner):
            raise NoSuchInterfaceException(interface_id=interface_id,
                    interface_name=interface_name, info="You're not the "
                    "owner of that particular interface.")
    if interface is None and interface_name and interface_name in interfaces_by_name:
        for test_interface in interfaces_by_name[interface_name]:
            if owner is None or ((not test_interface.special) and 
                    test_interface.connection.id == owner):
                interface = test_interface
                break
    if interface is None:
        raise NoSuchInterfaceException(interface_id=interface_id,
                interface_name=interface_name)
    return interface

def process_register_interface_command(message, sender, connection):
    command = MessageValue[message]
    name = command.name
    info = command.info
    print "Registering interface with name " + name
    interface = Interface(name, connection, info)
    interface.register()
    response_message, response = create_message_pair(protobuf.RegisterInterfaceResponse, message)
    response.id = interface.id
    connection.send(response_message)

def process_register_function_command(message, sender, connection):
    command = MessageValue[message]
    interface_id = command.interface_id
    interface_name = command.interface_name
    function_name = command.name
    try:
        interface = lookup_interface(interface_id, interface_name, sender)
    except NoSuchInterfaceException as e:
        connection.send_error(message, text=str(e))
        return
    interface.register_function(sender, function_name)
    response_message, response = create_message_pair(protobuf.RegisterFunctionResponse, message)
    connection.send(response_message)

def process_call_function_command(message, sender, connection):
    command = MessageValue[message]
    try:
        interface = lookup_interface(command.interface_id, command.interface_name)
        function = interface.lookup_function(function_name=command.function)
    except (NoSuchInterfaceException, NoSuchFunctionException) as e:
        connection.send_error(message, text=str(e))
        return
    if function.special:
        function.invoke_special(message, command, connection)
        return
    invoke_message, invoke_command = create_message_pair(protobuf.RunFunctionCommand,
            interface_id=interface.id, interface_name=interface.name,
            function=function.name, arguments=command.arguments)
    interface.connection.send(invoke_message)
    print ("Sending run command to " + str(interface.connection.id) +
            " with message id " + str(invoke_message.message_id) +
            " whose response is to be forwarded with id " + 
            str(message.message_id))
    interface.connection.pending_responses[invoke_message.message_id] = (
            sender, message.message_id)

def process_run_function_response(message, sender, connection):
    result = MessageValue[message]
    response, call_response = create_message_pair(protobuf.CallFunctionResponse,
            RESPONSE, return_value=result.return_value)
    if not message.message_id in connection.pending_responses:
        # Sporadic response received, just ignore it
        print ("Sporadic run response received from connection " + str(sender)
                + " for reported message id " + str(message.message_id))
        return
    response_connection_id, response_message_id = connection.pending_responses[message.message_id]
    del connection.pending_responses[message.message_id]
    response.message_id = response_message_id
    if response_connection_id in connection_map: # Make sure the connection
        # listening for the response is still alive
        response_connection = connection_map[response_connection_id]
        response_connection.send(response)


Interface("autobus", autobus_interface, None).register_special()

# MAIN CODE

server = Socket()
if len(sys.argv) > 1:
    server_port = int(sys.argv[1])
else:
    server_port = DEFAULT_PORT
print "Listening on port " + str(server_port)
server.bind(("", server_port))
server.listen(50)

Thread(target=process_event_queue).start()

print "\nAutobus has successfully started up."

try:
    while True:
        socket, address = server.accept()
        connection = Connection(socket)
        event_queue.put((connection.id, discard_args(connection.register)), block=True)
        connection.start()
except KeyboardInterrupt:
    print "KeyboardInterrupt received, shutting down"
    event_queue.put((None, None), block=True)
    print "Event queue has been notified to shut down"
except:
    print "Unexpected exception occurred in the main loop, shutting down. Stack trace:"
    print_exc()
    event_queue.put((None, None), block=True)
    print "Event queue has been notified to shut down"
server.close()

    




        



















