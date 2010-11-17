


from threading import Thread, RLock
from Queue import Queue
from libautobus import read_fully, discard_args, create_message
from libautobus import InputThread, OutputThread, get_next_id
from libautobus import COMMAND, RESPONSE, NOTIFICATION
from libautobus import DEFAULT_PORT, NoSuchInterfaceException
from libautobus import NoSuchFunctionException, encode_object, decode_object
from libautobus import get_function_doc
from libautobus.message_types import *
from struct import pack, unpack
from json import dumps, loads
from traceback import print_exc
from socket import socket as Socket, SHUT_RDWR, SOL_SOCKET, SO_REUSEADDR
from functools import partial
import re
import sys
import inspect
import os
from autobus import processors, special_interface, special_objects

print """\
Autobus, a message bus that's kind of a cross of D-Bus and RPC, written
by Alexander Boyd (a.k.a. javawizard or jcp)
"""

autobus_interface = None
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


def register_event_listener(interface_name, event_name, connection_id):
    """
    Adds a listener on the specified event. This only modifies the global listener
    map; this does not modify the connection's list of listeners., and this does
    not cause any RegisterListenerResponse to be sent back to the client.
    """
    event_spec = interface_name, event_name
    if event_spec not in listener_map:
        listener_map[event_spec] = []
    listener_map[event_spec].append(connection_id)

def deregister_event_listener(interface_name, event_name, connection_id):
    """
    Same as register_event_listener, but deregisters a listener previously added.
    """
    event_spec = interface_name, event_name
    listener_map[event_spec].remove(connection_id)
    if len(listener_map[event_spec]) == 0:
        del listener_map[event_spec]

def register_object_watch(interface_name, object_name, connection_id):
    """
    Adds a watch on the specified object. This only modifies the global watch
    map; this does not modify the connection's list of watches., and this does
    not cause any WatchObjectResponse to be sent back to the client.
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
    """
    Starts processing the event queue. This function returns when (None, None)
    is pushed onto the event queue. This function generally should be started
    in a new thread.
    """
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
            os._exit(1)
    print "Shutting down connections..."
    connection_count = len(connection_map)
    for connection in connection_map.values():
        connection.shutdown_and_deregister()
    print str(connection_count) + " connection" + ("s" if connection_count
            != 1 else "") + " have been shut down."

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
    
    def invoke_special(self, message, connection):
        args = message["arguments"]
        args = [decode_object(arg) for arg in args]
        try:
            return_value = self.special_target(*args)
        except Exception as e:
            print_exc()
            return_value = e
        result = encode_object(return_value)
        response = create_message(CallFunctionResponse,
                message, return_value=result)
        connection.send(response)

class Event(object):
    def __init__(self, sender, name, doc):
        self.name = name
        self.sender = sender
        self.doc = doc
        # TODO: allow special events
    
    def notify(self, arguments):
        """
        Sends a message to all connections listening for this event containing
        the specified arguments, which should be passed into this function as
        a (possibly empty) list or tuple.
        """
        listener_spec = (self.interface.name, self.name)
        if listener_spec in listener_map:
            message = create_message(FireEventCommand,
                    NOTIFICATION, interface_name=self.interface.name,
                    event_name=self.name, arguments=arguments)
            encoded_message = dumps(message)
            for connection_id in listener_map[listener_spec]:
                connection = connection_map[connection_id]
                connection.send(encoded_message)

    
class Object(object):
    def __init__(self, sender, name, doc, value):
        """
        Initializes this object. sender is the id of the connection that
        created this object. Name and doc are the object's name and documentation,
        respectively. value is the object's initial value.
        """
        self.name = name
        self.interface = None # Interface sets this when an object is registered
        # to an instance of it, so we don't have to
        self.sender = sender
        self.doc = doc
        self._value = value # This is an instance of protobuf.Instance
    
    def set_and_notify(self, new_value):
        """
        Sets this object's value to the specified value and sends a message to
        all connections watching the object with the new value. 
        """
        self._value = new_value
        self.notify()
    
    def notify(self):
        """
        Sends a message to all connections watching this object containing the
        object's current value. This only calls self.get() once, so it's not
        generally processor-intensive even when lots of connections are
        listening for changes to this object.
        """
        watch_spec = (self.interface.name, self.name)
        if watch_spec in watch_map:
            current_value = self.get()
            message = create_message(SetObjectCommand,
                    NOTIFICATION, interface_name=self.interface.name,
                    object_name=self.name, value=current_value)
            encoded_message = dumps(message)
            for connection_id in watch_map[watch_spec]:
                connection = connection_map[connection_id]
                connection.send(encoded_message)
    
    def get(self):
        """
        Returns the current value of the object. This is used instead of
        self._value to allow for subclasses to provide custom values for the
        object. VirtualObject in particular makes use of this.
        
        The return value must be an instance of protobuf.Instance.
        """
        return self._value

class VirtualObject(Object):
    """
    A subclass of Object that allows the object to have a value computed
    whenever it is requested. This is for server-side use only, and is used to
    implement most objects on the Autobus interface.
    """
    def __init__(self, name, doc, accessor):
        """
        Initializes this object. Name, and doc are the same as on
        the Object constructor. Accessor is a function that takes no
        arguments and returns the object's current value as a normal Python
        object. The resulting object will be converted to an encoded instance
        before being returned from the object's get() function.
        """
        Object.__init__(self, 0, name, doc, None)
        self.accessor = accessor
    
    def set_and_notify(self, new_value):
        raise Exception("VirtualObjects cannot have their value set")
    
    def get(self):
        value = self.accessor()
        return encode_object(value)

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
        # Notifications
        if self is not autobus_interface:
            autobus_interface.notify_object("interface_count")
            autobus_interface.notify_object("interfaces")
            autobus_interface.notify_object("interface_items")
    
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
        # Notifications
        if self is not autobus_interface:
            autobus_interface.notify_object("interface_count")
            autobus_interface.notify_object("interfaces")
            autobus_interface.notify_object("interface_items")
    
    def register_function(self, sender, function_name, doc, notify=True):
        if function_name in self.function_map:
            raise KeyError()
        print ("Registering function " + function_name + " from " + 
                str(sender) + " on " + self.name) 
        function = Function(sender, function_name, doc)
        self.function_map[function_name] = function
        if notify and (self is not autobus_interface):
            autobus_interface.notify_object("interface_items")
        return function
    
    def register_event(self, event):
        if event.name in self.event_map:
            raise KeyError(event.name)
        print ("Registering event " + event.name + " from " + 
                str(event.sender) + " on " + self.name)
        event.interface = self
        self.event_map[event.name] = event
        if self is not autobus_interface:
            autobus_interface.notify_object("interface_items")
    
    def create_and_register_event(self, sender, event_name, doc):
        event = Event(sender, event_name, doc)
        self.register_event(event)
        return event
    
    def register_object(self, object):
        if object.name in self.object_map:
            raise KeyError(object.name)
        print ("Registering object " + object.name + " from " + 
                str(object.sender) + " on " + self.name)
        object.interface = self
        self.object_map[object.name] = object
        if self is not autobus_interface:
            autobus_interface.notify_object("interface_items")
        object.notify()
    
    def create_and_register_object(self, sender, object_name, doc, value):
        object = Object(sender, object_name, doc, value)
        self.register_object(object)
        return object
    
    def fire_event(self, name, arguments):
        """
        Looks up the specified event, then calls its notify method with the
        specified arguments.
        """
        self.event_map[name].notify(arguments)
    
    def notify_object(self, name):
        """
        Looks up the specified object, then calls its notify method.
        """
        self.object_map[name].notify()
    
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
    
    def describe_functions(self):
        """
        Returns a list of maps in the same format that the list_functions
        function of the Autobus internal interface returns. In fact, that
        function delegates to this one once it's looked up the interface.
        """
        result = []
        if self.special:
            function_list = dir(self.connection)
            function_list = [function for function in function_list if
                    not function.startswith("_")]
            for function in function_list:
                result.append({"name": function, "special": True,
                        "doc": get_function_doc(self.connection,
                                function)})
        else:
            for function in self.function_map.values():
                result.append({"name": function.name,
                        "special": function.special, "doc": function.doc})
        return result
    
    def describe_events(self):
        return [{"name": event.name, "doc": event.doc} 
                for event in self.event_map.values()]
    
    def describe_objects(self):
        return [{"name": object.name, "doc": object.doc}
                for object in self.object_map.values()]

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
        self.listeners = [] # List of tuples, each of which contains an
        # interface name and an event name.
        self.watches = [] # List of tuples, each of which contains an
        # interface name and an object name.
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
            if isinstance(message, dict) and message["message_type"] is None:
                return
            self.message_queue.put(message, block=True)
    
    def send_new(self, *args, **kwargs):
        """
        Exactly the same as
        self.send(libautobus.create_message(*args, **kwargs)).
        """
        self.send(create_message(*args, **kwargs))
    
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
        for interface_name, event_name in self.listeners:
            deregister_event_listener(interface_name, event_name, self.id)
        for interface_name, object_name in self.watches:
            deregister_object_watch(interface_name, object_name, self.id)

def create_error_response(message_id, text):
    return create_message(ErrorResponse, RESPONSE, message_id=message_id, text=text)

def find_function_for_message(message):
    action = message["action"]
    function_name = "process_" + action
    if function_name not in vars(processors):
        raise Exception("Message is missing processor function "
                        + function_name)
    return vars(processors)[function_name]

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


def register_builtin_interface():
    global autobus_interface
    interface = Interface("autobus", special_interface.AutobusInterface(), None)
    autobus_interface = interface
    interface.register_special()
    for name in dir(special_objects):
        if name[0] != "_":
            function = getattr(special_objects, name)
            if inspect.isfunction(function):
                if function.__doc__ is None:
                    function_doc = ""
                else:
                    function_doc = inspect.getdoc(function)
                interface.register_object(VirtualObject(name,
                        function_doc, function))

# MAIN CODE

def main():
    register_builtin_interface()
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

    




        



















