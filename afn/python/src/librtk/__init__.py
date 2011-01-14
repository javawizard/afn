
from threading import Thread, RLock
from Queue import Queue, Empty
from traceback import print_exc, format_exc
import default_widget_schema, default_features
import libautobus
from concurrent import synchronized
from categories import TOPLEVEL, CONTAINER, WIDGET

"""
RTK is a remote widget library. It stands for Remote ToolKit, and it's similar
to X, with two differences: it reverses the roles of client and server used in
X, and it sends specific instructions about which widgets to create and how to
lay them out instead of lower-level graphics instructions. The result is that
RTK applications look native to the operating system that the viewer is run
on.

Since an RTK application is a server instead of a client, multiple users can
connect to the same application at once. Each one receives a different copy
of the application, but they all run in the same process so they can
inter-communicate. (With the threading protocol, each copy of the application
runs in its own thread, while with the async protocol, a single thread manages
them all and Python's asyncore library is used to manage communication with
the viewers.)

A reference implementation viewer is provided in the form of the rtkinter
module. This viewer uses Tkinter to display the application.

Various RTK examples are provided in the examples package. A simple
"Hello, world" application that shows a window titled "Hello" containing one
label whose text is "Hello, world!" could be written thus:

from librtk.protocols import ThreadedServer
def start_app(connection):
    w = connection.Window(title="Hello")
    w.close_request.listen(connection.close)
    l = connection.Label(w, text="Hello, world!")
server = ThreadedServer("", 6785, start_app)
server.start()

A viewer would then be started and connected to port 6785. rtkinter could be
used to view this application thus:

./run rtkinter rtk://localhost:6785

This can be run multiple times simultaneously, or run from a different machine
than the one that the application is running on.

If you want to experiment around with a single viewer from the Python REPL,
you can use the listen utility function in librtk to listen for one viewer to
connect and return the connection. This can be used like so:

from librtk import listen
connection = listen(6785) # This will return once you start up a viewer,
                               # such as rtkinter, pointing at it
w = connection.Window(title="Hello")
...etc...

Updates will be sent to the viewer immediately as each line is executed. So as
soon as you hit enter after typing connection.Window(title="Hello"), the
window will pop open in the viewer.

dir() can be used on the connection to find out what widgets are available.

RTK can also be used as a traditional widget toolkit by using... TODO: finish 
"""

global_lock = RLock()
locked = synchronized(global_lock)

class MissingFeatures(Exception):
    """
    Thrown by a validator if it decides that the client doesn't have enough
    features for the program to sanely run on it.
    """
    pass

class EventThread(Thread):
    """
    An event dispatch thread. You create one of these by passing in an
    instance of Queue (or any other object that has a no-argument get function)
    and then start it. It reads callable objects off of the queue and calls
    them until the item returned from the queue is None.
    """
    def __init__(self, queue=None):
        """
        Creates a new EventThread with the specified queue. If no queue is
        specified, a new one is created. The queue is then stored in the
        queue attribute on the event thread.
        
        This doesn't start the event thread; you need to remember to call
        start on the event thread to start it.
        """
        Thread.__init__(self)
        if queue is None:
            queue = Queue()
        self.queue = queue
    
    def run(self):
        item = self.queue.get()
        while item is not None:
            try:
                item()
            except:
                print_exc()
            item = self.queue.get()
    
    def schedule(self, function):
        """
        Same as self.queue.put(function).
        """
        self.queue.put(function)

class Event(object):
    """
    An event manager. Listeners can be added by calling the listen function.
    Listeners can be removed by calling the unlisten function. The replace
    function removes all listeners and then adds one specific listener, in
    essence replacing all of the current listeners with the one specified.
    
    Calling an instance of this class fires the event. All arguments passed
    to the invocation will be passed to each listener. If any of the
    listeners raise an exception, the exception's traceback will be printed
    to stdout, but the exception will be otherwise suppressed (and in
    particular, all remaining listeners will still be called).
    """
    def __init__(self):
        """
        Creates a new event.
        """
        self.listeners = []
    
    def listen(self, function):
        """
        Adds the specified function as a listener to this event. If the
        function is already present on this event, it will not be added twice.
        """
        if function not in self.listeners:
            self.listeners.append(function)
    
    def unlisten(self, function):
        """
        Removes the specified function from this event's list of listeners.
        """
        try:
            self.listeners.remove(function)
        except ValueError:
            pass
    
    def __call__(self, *args, **kwargs):
        """
        Fires all listeners registered to this event. The specified arguments
        (including keyword arguments) will be passed to all of the listeners.
        """
        for listener in self.listeners[:]: # Clone in case someone modifies us
            try:
                listener(*args, **kwargs)
            except:
                print_exc()

class Connection(object):
    @locked
    def __init__(self, protocol, connect_function, validators=[],
            use_default_widgets=True, custom_features=[]):
        self.protocol = protocol
        self.connect_function = connect_function
        self.validators = validators[:]
        if use_default_widgets:
            self.validators.append(default_validator)
        self.children = []
        self.widgets = {}
        self.schema = {}
        self.features = default_features.features + custom_features
        self.started = False
        self.handshake_finished = False
        self.closed = False
        self.pre_start_messages = []
        self.close_functions = []
        protocol.protocol_init(self)
    
    @locked
    def add_close_function(self, function):
        """
        Adds a function that will be called when this connection is closed.
        """
        self.close_functions.append(function)
    
    @locked
    def start(self):
        """
        Starts this connection. This should be called after the connection has
        been set up (all validators have been added with add_validator, etc).
        """
        if self.started:
            raise Exception("You can't call Connection.start twice.")
        self.started = True
        self.protocol.protocol_start()
        # If any messages were buffered up by self.protocol_receive before
        # we started, we'll process them now.
        messages = self.pre_start_messages
        self.pre_start_messages = None
        for message in messages:
            self.protocol_receive(message)
    
    @locked
    def send(self, packet):
        """
        Used by internal RTK code to send a packet to the client. The packet
        should be a dict ready to be put into json.dumps.
        """
        self.protocol.protocol_send(packet)
    
    @locked
    def add_validator(self, validator):
        """
        Adds a new validator function. TODO: more on this later.
        """
        self.validators.append(validator)
    
    @locked
    def schedule(self, function):
        """
        Schedules a function to be run as soon as possible. The function will
        be put on the event queue, so no incoming changes will be processed
        while the function is running.
        """
        self.protocol.protocol_event_ready(function)
    
    @locked
    def fatal_error(self, message):
        # TODO: Right now, errors that are most likely the fault of the
        # programmer and errors that result from the client messing up are
        # both printed by this function. Perhaps split them out so the
        # application developer knows what to look for to know if they have
        # potential problems with their application.
        print "FATAL CONNECTION ERROR: " + message
        self.send({"action": "drop", "text": message})
        self.close(True)
    
    @locked
    def protocol_receive(self, data):
        """
        Called by subclasses of Connection when they have new data for
        Connection to process. data should be the return value of json.loads
        or a compatible object.
        """
        if not self.started: # Haven't started yet, so buffer up messages
            # until when someone calls self.start()
            self.pre_start_messages.append(data)
            return
        if not self.handshake_finished: # Perform handshake
            if not data["action"] == "connect":
                self.fatal_error("First message must be a connect message")
                return
            features = data["features"]
            application = data.get("application", None)
            self.application = application
            try:
                for validator in self.validators:
                    validator(features, self.schema, self)
            except MissingFeatures, e:
                self.fatal_error("Missing features: " + str(e))
                return
            self.send({"action": "accept", "features": self.features})
            self.handshake_finished = True
            self.create_widget_constructors()
            try:
                self.connect_function(self)
            except:
                print_exc()
                self.fatal_error("Exception in connect function")
                return
            return
        # If this is directed to a widget, we'll look it up and dispatch it.
        # If it's not, we'll ignore it for now and issue a message about it,
        # since there aren't any messages at present that don't target widgets.
        #
        # We'll do the actual dispatching on the event queue so that widgets
        # aren't modified while a listener is running. 
        if "id" in data:
            def dispatch():
                widget = self.widgets.get(data["id"], None)
                if widget is None: # Ignore messages targeted to nonexistent
                    # widgets; most likely the widget was deleted just before
                    # the event arrived across the wire.
                    return
                # Dispatch the message.
                widget.dispatch(data)
            self.schedule(dispatch)
        elif data["action"] == "error":
            print "CLIENT REPORTED AN ERROR: " + data["text"]
        elif data["action"] == "drop":
            print "CLIENT REPORTED A FATAL ERROR: " + data["text"]
            self.close(hard=True)
        else:
            print ("Message (with action " + data["action"] + ") ignored "
                    "because it does not have a target id")
    
    @locked
    def protocol_connection_lost(self):
        """
        Called by subclasses of Connection when the connection has been lost.
        If Connection previously called protocol_close (which subclasses
        themselves implement), the connection is not required (but is allowed)
        to call this function. If protocol_close has not been called, this
        will cause it to be called.
        """
        self.close(True)
    
    @locked
    def close(self, hard=False):
        """
        Closes this connection. If hard is True, a hard close is performed,
        which essentially calls protocol_close without doing anything else. If
        hard is False (the default), various cleanup actions (such as
        destroying all of this connection's widgets) are performed first.
        
        If the connection has already been closed, this does nothing. 
        """
        if self.closed:
            return
        self.closed = True
        if not hard:
            for widget in self.children[:]:
                widget.destroy()
        self.protocol.protocol_close()
        for function in self.close_functions:
            try:
                function()
            except:
                print_exc()
    
    def add_child(self, child):
        self.children.append(child)
        child.send_create()
    
    def get_child_index(self, child):
        return self.children.index(child)
    
    def create_widget_constructors(self):
        """
        Called by internal code to create self.Window, self.Button, etc.
        These are generated from the widget schema. External code should not
        use this.
        """
        for name, value in self.schema.items():
            if value[1] == TOPLEVEL:
                setattr(self, name, ResidentToplevelConstructor(value[0], self))
            else:
                setattr(self, name, ResidentWidgetConstructor(value[0]))

class ResidentWidget(object):
    @locked
    def __init__(self, type, parent, **kwargs):
        object.__setattr__(self, "resident_ready", False)
        self.type = type
        self.id = libautobus.get_next_id()
        if isinstance(parent, Connection):
            self.parent = None
            self.connection = parent
        else:
            self.parent = parent
            self.connection = parent.connection
        self.children = [] # Create even for non-container types to make some
        # iteration stuff simpler. This will just be empty for such types.
        self.server_name, self.category = self.connection.schema[type][0:2]
        if self.parent is None and self.category != TOPLEVEL:
            raise Exception("Only a toplevel can have the connection as "
                    "its parent.")
        if self.parent is not None and self.category == TOPLEVEL:
            raise Exception("Toplevels must have a connection as their "
                    "parent, not another container or widget.")
        if self.category != TOPLEVEL and self.parent.category == WIDGET:
            raise Exception("Containers and widgets can only be added to "
                    "toplevels and containers, not other widgets.")
        (self.widget_schema, self.layout_schema, self.state_schema,
        self.call_schema, self.event_schema) = convert_widget_schema_to_maps(
                self.connection.schema[type])
        for key, (writable, default_value) in self.widget_schema.items():
            if default_value is None and key not in kwargs:
                raise Exception("Widget property " + key + " is required to "
                        "construct a widget of type " + type + ", but this "
                        "property was unspecified.")
        if self.parent is not None:
            self.parent.validate_layout_attributes(kwargs)
        # At this point we've validated everything, so we can do ahead and
        # start setting stuff up.
        self.connection.widgets[self.id] = self
        self.widget_properties = dict([(k, kwargs.get(k, d)) 
                for k, (w, d) in self.widget_schema.items()])
        if self.parent is not None:
            self.layout_properties = dict([(k, kwargs.get(k, d)) 
                    for k, (w, d) in self.parent.layout_schema.items()])
        else:
            self.layout_properties = {}
        self.state_properties = dict([(k, d)
                for k, (d,) in self.state_schema.items()])
        self.state_events = dict([(k, Event()) for k in self.state_schema.keys()])
        self.events = dict([(k, Event()) for k in self.event_schema.keys()])
        self.owner.add_child(self)
        self.resident_ready = True
    
    @property # Doesn't need to be locked since self.parent and self.connection
    # never change after construction
    def owner(self):
        """
        The owner of this widget. This is the same as the widget's parent, or
        the connection if this widget is a toplevel widget.
        """
        return self.parent or self.connection
    
    @locked
    def validate_layout_attributes(self, attributes):
        """
        Validates that the layout attributes required by this widget, which
        must be a container or a toplevel, are present in the specified map of
        attributes.
        
        This is used by ResidentWidget for a child widget to validate that it
        contains all of the layout attributes needed by its parent-to-be.
        Outside code generally shouldn't call this.
        """
        for key, (writable, default_value) in self.layout_schema.items():
            if default_value is None and key not in attributes:
                raise Exception("Layout property " + key + " is required for "
                        + "children of " + self.category + "s of type "
                         + self.type + ", but this property was unspecified.")
    
    @locked
    def add_child(self, child):
        """
        Requests that this container or toplevel adopt the specified child.
        This class implements this to add the child to the list of child
        widgets and call the child's send_add function.
        
        This generally should not be called by outside code. It's called
        implicitly when constructing a ResidentWidget instance, so you don't
        need to call it yourself.
        """
        self.children.append(child)
        child.send_create()
    
    @locked
    def send_create(self):
        """
        Sends a message to the client telling it to perform the actual
        creation of this widget. Outside code generally shouldn't call this;
        it's called by a widget's parent-to-be in add_child.
        """
        message = {"action": "create", "id": self.id, "type": self.type,
                "index": self.owner.get_child_index(self), "p_widget":
                self.widget_properties, "p_layout": self.layout_properties}
        if self.parent is not None:
            message["parent"] = self.parent.id
        self.connection.send(message)
    
    @locked
    def get_child_index(self, child):
        """
        Returns the index at which a particular child is located in this
        container or toplevel. If the specified widget is not a child of this
        widget, ValueError will be raised.
        """
        return self.children.index(child)
    
    @locked
    def dispatch(self, message):
        """
        Dispatches an event or a state property change to this widget. This
        must only be called on the event queue. This will take care of
        notifying any interested listeners.
        """
        if message["action"] == "set_state":
            properties = message["properties"]
            self.state_properties.update(properties)
            for k in properties.keys():
                self.state_events[k]()
        elif message["action"] == "event":
            self.events[message["name"]](*message["args"])
    
    @locked
    def __getattr__(self, name):
        if not object.__getattribute__(self, "resident_ready"):
            return object.__getattribute__(self, name)
        if name in self.widget_schema:
            return self.widget_properties[name]
        elif self.parent is not None and name in self.parent.layout_schema:
            return self.layout_properties[name]
        elif name in self.state_schema:
            return self.state_properties[name]
        elif name in self.call_schema:
            raise Exception("Calls aren't supported yet.")
        elif name in self.event_schema:
            return self.events[name]
        elif (name.endswith("_changed") and
                name[:-len("_changed")] in self.state_schema):
            return self.state_events[name]
        # We don't need to delegate to object since __getattr__ is only
        # called if Python can't find the attribute any other way
        raise AttributeError("Widget of type " + self.type + 
                " has no property " + name)
    
    @locked
    def __setattr__(self, name, value):
        if not object.__getattribute__(self, "resident_ready"):
            object.__setattr__(self, name, value)
            return
        if name in self.widget_schema:
            writable, default = self.widget_schema[name]
            if not writable:
                raise Exception("You can't modify the widget property " + name
                        + " on a widget of type " + self.type)
            self.widget_properties[name] = value
            self.send_set_widget(name)
        elif self.parent is not None and name in self.parent.layout_schema:
            writable, default = self.parent.layout_schema[name]
            if not writable:
                raise Exception("You can't modify the layout property " + name
                        + " on a widget of type " + self.type + " contained "
                        " in a parent of type " + self.parent.type)
            self.layout_properties[name] = value
            self.parent.send_set_layout(self, name)
        elif name in self.state_schema:
            raise Exception("You can't modify the state property " + name
                    + " on a widget of type " + self.type + ". State "
                    "attributes can only be modified by the client.")
        elif name in self.call_schema:
            raise Exception("You can't set values for properties "
                    "corresponding to calls on widgets. You just tried to "
                    "set the property " + name + " on a widget of type "
                    + self.type + ".")
        elif name in self.event_schema:
            raise Exception("Events can't be set for now. In the future, this "
                    "will be allowed, with the result that all listeners on "
                    "the specified event will be removed and replaced with "
                    "the value you're assigning to this property.")
        elif (name.endswith("_changed") and
                name[:-len("_changed")] in self.state_schema):
            raise Exception("State events can't be set for now. In the future, this "
                    "will be allowed, with the result that all listeners on "
                    "the specified state event will be removed and replaced with "
                    "the value you're assigning to this property.")
        else:
            object.__setattr__(self, name, value)
    
    @locked
    def send_set_widget(self, name):
        self.connection.send({"action": "set_widget", "id": self.id, "properties":
                {name: getattr(self, name)}})
    
    @locked
    def send_set_layout(self, child, name):
        self.connection.send({"action": "set_layout", "id": self.id, "properties":
                {name: getattr(child, name)}})
    
    @locked
    def destroy(self):
        for child in self.children[:]:
            child.destroy()
        self.connection.send({"action": "destroy", "id": self.id})
        # TODO: perhaps route this through the parent
        self.owner.children.remove(self)
        del self.connection.widgets[self.id]
    
    def __getitem__(self, item):
        # TODO: in the future, perhaps allow this as a shortcut for getattr
        # for string items
        return self.children[item] # This will raise an IndexError for us if
        # the specified child doesn't exist so we don't need to
    
    def __len__(self):
        return len(self.children)
    
    def __nonzero__(self):
        return True
    
    def __str__(self):
        return "<ResidentWidget " + str(self.id) + ": " + self.type + ">"
    
class ResidentWidgetConstructor(object):
    def __init__(self, type):
        self.type = type
    
    def __call__(self, parent, **kwargs):
        return ResidentWidget(self.type, parent, **kwargs)
    
    def __str__(self):
        return "<ResidentWidgetConstructor for widget type " + self.type + ">"
    
    __repr__ = __str__

class ResidentToplevelConstructor(object):
    def __init__(self, type, connection):
        self.type = type
        self.connection = connection
    
    def __call__(self, **kwargs):
        return ResidentWidget(self.type, self.connection, **kwargs)
    
    def __str__(self):
        return "<ResidentToplevelConstructor for widget type " + self.type + ">"
    
    __repr__ = __str__

def default_validator(features, schema, connection):
    schema.update(default_widget_schema.schema)

def convert_widget_schema_to_maps(schema):
    """
    Converts the schema for a particular widget (the value of a key in the
    overall schema) to a five-element list, with each element corresponding
    to a property type, of maps, each map containing the name of a property
    of that type as the key and a list of all of the info about the property
    (essentially, all of the items in its list in the schema after the
    property's name) as the value.
    """
    return [dict([(k[0], k[1:]) for k in i]) for i in schema[2:7]]

def listen(port, localhost_only=True, handshake_timeout=10):
    """
    Listens on the specified port for a single connection from a single
    viewer. The connection's handshake is then performed and the connection
    object returned.
    
    If localhost_only is True (the default), only connections on the loopback
    adapter will be allowed. If it's False, connections on any interface will
    be allowed.
    
    handshake_timeout is the number of seconds to wait after a connection is
    received for the handshake to be performed by the client. This defaults to
    10 seconds. If the handshake is not performed within this many seconds,
    the connection will be closed and an exception thrown. This does not
    affect actually listening for a connection before one is established;
    this function will wait indefinitely for this to happen.
    """
    from protocols import ThreadedProtocol
    import socket
    server = socket.socket()
    server.bind(("127.0.0.1" if localhost_only else "", port))
    server.listen(1)
    client, _ = server.accept()
    protocol = ThreadedProtocol(client)
    queue = Queue()
    def connected(connection):
        queue.put(None)
    connection = Connection(protocol, connected)
    connection.start()
    try:
        queue.get(block=True, handshake_timeout)
    except Empty:
        try:
            connection.close()
        except:
            pass
        raise Exception("Handshake not completed within the specified number "
                "of seconds (which was " + str(handshake_timeout) + ")")
    return connection







































