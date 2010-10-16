


from threading import Thread, RLock
from Queue import Queue
from libautobus import ProtoMultiAccessor, read_fully, discard_args
from libautobus import InputThread, OutputThread, get_next_id
from libautobus import COMMAND, RESPONSE, NOTIFICATION, create_message_pair
from libautobus import DEFAULT_PORT, NoSuchInterfaceException
from libautobus import NoSuchFunctionException, encode_object, decode_object
from libautobus import get_function_doc
from struct import pack, unpack
import autobus_protobuf.autobus_pb2 as protobuf
from traceback import print_exc
from socket import socket as Socket, SHUT_RDWR, SOL_SOCKET, SO_REUSEADDR
from functools import partial
import re
import sys
import inspect

print """\
Autobus, a message bus that's kind of a cross of D-Bus and RPC, written
by Alexander Boyd (a.k.a. javawizard or jcp)
"""

connection_map = {} # connection ids to connection objects
interface_map = {} # interface names to interfaces
listener_map = {} # Maps tuples of interface names and function names to lists
# of connection ids representing connections that should be notified when the
# specified listener is fired
watch_map = {} # Same as listener_map, but represents connections that should
# receive an object's value when it changes
event_queue = Queue() # A queue of tuples, each of which consists of a
# connection id and either a protobuf Message object or None to indicate that
# the connecton was lost and so should be shut down
processed_message_count = 0

# We're breaking naming conventions with these two because I'm paranoid of
# accidentally conflicting them if I use the regular naming convention
MessageValue = ProtoMultiAccessor(protobuf.Message, "value")
InstanceValue = ProtoMultiAccessor(protobuf.Instance, "value")

def register_object_watch(interface_name, object_name, connection_id):
    """
    Adds a watch on the specified object. This only modifies the global watch
    map; this does not modify the connection's list of watches.
    """
    object_spec = interface_name, object_name
    if object_spec not in watch_map:
        watch_map[object_spec] = []
    watch_map[object_spec].append(connection_id)

def deregister_object_watch(interface_name, object_name, connection_id):
    """
    Same as register_object_watch, but deregisters a watch previously added.
    """
    object_spec = interface_name, object_name
    watch_map[object_spec].remove(connection_id)
    if len(watch_map[object_spec]) == 0:
        del watch_map[object_spec]

def process_event_queue():
    global processed_message_count
    while True:
#        print "Waiting for events..."
        sender, message = event_queue.get(block=True)
        if sender is None: # pushed on at KeyboardInterrupt to shut down the event queue
#            print "Shutdown event received"
            break
        connection = connection_map.get(sender, None)
#        print ("Event " + str(message) + " received for sender " + str(sender)
#                + " and connection " + str(connection))
        processed_message_count += 1
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
    """
    An interface built-in to the Autobus server that provides information about
    the server, such as the interfaces that are currently registered. To see a
    list of functions that are on this interface, call the function
    list_functions on this interface, passing in the string "autobus". The
    resulting list should provide plenty of information.
    """
    def list_interfaces(self):
        """
        Returns a list of all interfaces currently registered to the autobus
        server. The return value is a list of maps, with each map representing
        one interface. Each map has the following keys:
        
        owner: The numeric id of the connection that registered this interface.
        
        name: The name of the interface.
        
        info: The info object associated with the interface.
        
        special: True if this interface is special (I.E. registered by
        server-side code instead of by a client), false if it is a normal
        interface.
        
        doc: The documentation of the interface if the client supplied any such
        documentation, or the empty string if they didn't.
        """
        result = []
        for interface in interface_map.values():
            result.append({"owner": interface.connection.id if not
                    interface.special else 0, "name": interface.name,
                    "special": interface.special, "doc": interface.doc})
        return result
    
    def list_functions(self, interface_name):
        """
        Returns a list of all functions currently registered to the specified
        interface. If there's no such interface, an exception will be thrown.
        This function works correctly on special interfaces as well as normal
        interfaces.
        
        The return value is similar to the return value of list_interfaces().
        Each map within the list that's returned represents a function on the
        interface. 
        """
        result = []
        try:
            interface = interface_map[interface_name]
        except KeyError:
            raise Exception("No such interface.")
        if interface.special:
            function_list = dir(interface.connection)
            function_list = [function for function in function_list if
                    not function.startswith("_")]
            for function in function_list:
                result.append({"name": function, "special": True,
                        "doc": get_function_doc(interface.connection,
                                function)})
        else:
            for function in interface.function_map.values():
                result.append({"name": function.name,
                        "special": function.special, "doc": function.doc})
        return result
    
    def list_objects(self, interface_name):
        """
        Same as list_functions, but returns a list of objects available on this
        interface. 
        """
    
    def get_processed_message_count(self):
        """
        Returns the number of messages that Autobus has received and processed
        since it started up. This does not count messages that Autobus has
        sent; only inbound messages are counted.
        """
        return processed_message_count

autobus_interface = AutobusInterface()

class Function(object):
    """
    Represents a function registered on an interface.
    """
    def __init__(self, sender, name, doc, special_target=None):
        self.name = name
        self.sender = sender
        self.doc = doc
        self.special_target = special_target
        self.special = special_target != None
        if self.special:
            self.doc = inspect.getdoc(special_target)
            if self.doc is None:
                self.doc = ""
    
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

class Event(object):
    def __init__(self, sender, name, doc):
        self.name = name
        self.sender = sender
        self.doc = doc
        # TODO: allow special events
    
class Object(object):
    def __init__(self, sender, interface, name, doc, value):
        self.name = name
        self.interface = interface
        self.sender = sender
        self.doc = doc
        self.value = value # This is an instance of protobuf.Instance
    
    def set_and_notify(self, new_value):
        """
        Sets this object's value to the specified value and sends a message to
        all connections watching the object with the new value. 
        """
        self.value = new_value
        self.notify()
    
    def notify(self):
        """
        Sends a message to all connections watching this object containing the
        object's current value.
        """
        watch_spec = (self.interface.name, self.name)
        if watch_spec in watch_map:
            for connection_id in watch_map[watch_spec]:
                connection = connection_map[connection_id]
                message, set_message = create_message_pair(protobuf.SetObjectCommand,
                        NOTIFICATION, interface_name=self.interface.name,
                        object_name=self.name, value=self.value)
                connection.send(message)

class Interface(object):
    """
    Represents an interface registered by a particular connection. Interfaces
    have an id, a name, and a reference to the connection that registered the
    interface. They also have a dict mapping names of functions to the
    corresponding function objects, and they have a single Instance
    representing more information about the interface.
    """
    def __init__(self, name, connection, doc):
        """
        Creates a new interface object. This does not register it with all of
        the global dicts nor the connection's dicts; register() must be called
        to do that.
        """
        self.name = name
        self.connection = connection
        self.doc = doc
        self.function_map = {} # Maps function names to Function instances
        self.event_map = {} # Maps event names to Event instances
        self.object_map = {} # Maps object names to Object instances
        self.special = False
    
    def register(self):
        """
        Registers this interface with the global dicts and the connection's
        dicts. This must only be called on the event thread.
        """
        if self.name in interface_map:
            raise KeyError()
        interface_map[self.name] = self
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
        self.doc = inspect.getdoc(self.connection)
        if self.doc is None:
            self.doc = ""
    
    def deregister(self):
        """
        The opposite of register(): deregisters this interface.
        """
        print "Deregistering interface with name " + self.name
        del interface_map[self.name]
        self.connection.interfaces.remove(self)
        for object_name in self.object_map:
            self.object_map[object_name].set_and_notify(encode_object(None))
    
    def register_function(self, sender, function_name, doc):
        if function_name in self.function_map:
            raise KeyError()
        print ("Registering function " + function_name + " from " + 
                str(sender) + " on " + self.name) 
        function = Function(sender, function_name, doc)
        self.function_map[function_name] = function
        return function
    
    def register_object(self, sender, object_name, doc, value):
        if object_name in self.object_map:
            raise KeyError()
        print ("Registering object " + object_name + " from " + 
                str(sender) + " on " + self.name)
        object = Object(sender, self, object_name, doc, value)
        self.object_map[object_name] = object
        object.notify()
        return object
    
    def lookup_function(self, function_name):
        """
        Looks up the function with the specified id or name. If this interface
        is special, this function requires the name to be specified, and a
        special function object wrapping the function in question will be
        returned.
        """
        if self.special:
            if function_name.startswith("_"): # Don't allow functions whose
                # names start with underscores
                raise NoSuchFunctionException(function_name)
            try:
                return Function(None, function_name, None, getattr(self.connection, function_name))
            except AttributeError:
                raise NoSuchFunctionException(function_name)
        if function_name in self.function_map:
            return self.function_map[function_name]
        raise NoSuchFunctionException(function_name)

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
        self.listeners = []
        self.watches = [] # List of tuples, each of which contains an
        # interface name, an object name, and a listener id.
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
        for interface_name, object_name in self.watches:
            deregister_object_watch(interface_name, object_name, self.id)

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

def lookup_interface(interface_name, owner=None):
    """
    Looks up an interface with the specified name. If there is no such
    interface, a NoSuchInterfaceException will be raised.
    
    If owner is not None, it should be the id of a connection. Only an
    interface registered by this connection will be returned.
    """
    if interface_name not in interface_map:
        raise NoSuchInterfaceException(interface_name)
    else:
        interface = interface_map[interface_name]
        if owner is not None and (interface.special or 
                interface.connection.id != owner):
            raise NoSuchInterfaceException(interface_name,
                    info="You're not the owner of that particular interface.")
        return interface

def process_register_interface_command(message, sender, connection):
    command = MessageValue[message]
    name = command.name
    doc = command.doc
    print "Registering interface with name " + name
    interface = Interface(name, connection, doc)
    try:
        interface.register()
    except KeyError:
        connection.send_error(message, "An interface with the same name "
                "is already registered.")
        return
    response_message, response = create_message_pair(protobuf.RegisterInterfaceResponse, message)
    connection.send(response_message)

def process_register_function_command(message, sender, connection):
    command = MessageValue[message]
    interface_name = command.interface_name
    function_name = command.name
    doc = command.doc
    try:
        interface = lookup_interface(interface_name, sender)
    except NoSuchInterfaceException as e:
        connection.send_error(message, text=str(e))
        return
    try:
        interface.register_function(sender, function_name, doc)
    except KeyError:
        connection.send_error(message, "A function with that name has already "
                "been registered on that interface.")
        return
    response_message, response = create_message_pair(protobuf.RegisterFunctionResponse, message)
    connection.send(response_message)

def process_call_function_command(message, sender, connection):
    command = MessageValue[message]
    try:
        interface = lookup_interface(command.interface_name)
        function = interface.lookup_function(command.function)
    except (NoSuchInterfaceException, NoSuchFunctionException) as e:
        connection.send_error(message, text=str(e))
        return
    if function.special:
        function.invoke_special(message, command, connection)
        return
    invoke_message, invoke_command = create_message_pair(protobuf.RunFunctionCommand,
            interface_name=interface.name, function=function.name,
            arguments=command.arguments)
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
                + " for reported message id " + str(message.message_id) + 
                ". It will be ignored.")
        return
    response_connection_id, response_message_id = connection.pending_responses[message.message_id]
    del connection.pending_responses[message.message_id]
    response.message_id = response_message_id
    if response_connection_id in connection_map: # Make sure the connection
        # listening for the response is still alive
        response_connection = connection_map[response_connection_id]
        response_connection.send(response)

def process_register_object_command(message, sender, connection):
    command = MessageValue[message]
    interface_name = command.interface_name
    object_name = command.object_name
    doc = command.doc
    value = command.value
    try:
        interface = lookup_interface(interface_name, sender)
    except NoSuchInterfaceException as e:
        connection.send_error(message, text=str(e))
        return
    try:
        interface.register_object(sender, object_name, doc, value)
    except KeyError:
        connection.send_error(message, "An object with that name has already "
                "been registered on that interface.")
        return
    response_message, response = create_message_pair(protobuf.RegisterObjectResponse, message)
    connection.send(response_message)

def process_watch_object_command(message, sender, connection):
    command = MessageValue[message]
    interface_name = command.interface_name
    object_name = command.object_name
    register_object_watch(interface_name, object_name, sender)
    connection.watches.append((interface_name, object_name))
    try:
        interface = lookup_interface(interface_name)
        object = interface.object_map[object_name]
        object_value = object.value
    except (NoSuchInterfaceException, KeyError): # Object doesn't exist yet
        object_value = encode_object(None)
    response_message, response = create_message_pair(protobuf.WatchObjectResponse,
            message, interface_name=interface_name, object_name=object_name,
            value=object_value)
    connection.send(response_message)

def process_set_object_command(message, sender, connection):
    command = MessageValue[message]
    interface_name = command.interface_name
    object_name = command.object_name
    value = command.value
    try:
        interface = lookup_interface(interface_name, sender)
    except NoSuchInterfaceException as e:
        connection.send_error(message, text=str(e))
        return
    if object_name not in interface.object_map:
        connection.send_error(message, "You need to register the object "
                "with Autobus before you can set its value.")
        return
    interface.object_map[object_name].set_and_notify(value)

Interface("autobus", autobus_interface, None).register_special()

# MAIN CODE

server = Socket()
if len(sys.argv) > 1:
    server_port = int(sys.argv[1])
else:
    server_port = DEFAULT_PORT
print "Listening on port " + str(server_port)
server.setsockopt(SOL_SOCKET, SO_REUSEADDR, 1)
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

    




        



















