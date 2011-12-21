
from threading import Thread, RLock
from Queue import Queue, Empty
from traceback import print_exc, format_exc
from schema import default as default_widget_schema
import weakref
import default_features
import libautobus
from concurrent import synchronized
from schema.categories import TOPLEVEL, CONTAINER, WIDGET
import textwrap
import pydoc

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

RTK is thread-safe. Multiple threads can access and modify RTK widgets
concurrently. Instances of an application can even pass connections around and
modify widgets on other connections in the same process.

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

To get help with a particular widget from the command line, use
some_widget.help(). This will open an interactive help console similar to
Python's built-in help function that shows information about that widget and
what attributes it supports. This function is available on both widget
constructors (connection.Window.help(), for example) and widget instances
(w = connection.Window(...); w.help(), for example). Since the help function
derives its information solely from the widget schema (which is available as a
field on the connection object), it should be possible to write an HTML
documentation generator for this; this is something I plan on doing in the
future.

RTK can also be used as a traditional widget toolkit by using... TODO: finish 
"""

from afn.utils.listener import Event

global_lock = RLock()
text_doc = pydoc.TextDoc()
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
        self.combined_class_cache = weakref.WeakValueDictionary()
        self.layout_classes = {}
        self.features = default_features.features + custom_features
        self.started = False
        self.handshake_finished = False
        self.closed = False
        self.pre_start_messages = []
        self.close_functions = []
        self.client_features = None
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
            self.client_features = features
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
            if value.type == TOPLEVEL:
                setattr(self, name, ResidentWidgetConstructor(value, value[0], self))
            else:
                setattr(self, name, ResidentWidgetConstructor(value, value[0]))
    
    def __getitem__(self, item):
        return self.children[item]
    
    def __len__(self):
        return len(self.children)
    
    def __nonzero__(self):
        return True
    
class ResidentWidget(object):
    """
    A client-side widget. Instances of this class correspond to specific
    widgets being used on the client side.
    """
    
    @locked
    def __init__(self, type, parent, **kwargs):
        object.__setattr__(self, "_resident_ready", False)
        self.type = type
        self.id = libautobus.get_next_id()
        if isinstance(parent, Connection):
            self.parent = None
            self.connection = parent
        else:
            self.parent = parent
            self.connection = parent.connection
        self._schema = self.connection.schema[type]
        self.children = [] # Create even for non-container types to make some
        # iteration stuff simpler. This will just be empty for such types.
        self.server_name, self.doc, self.category = self._schema[0:3]
        if self.parent is None and self.category != TOPLEVEL:
            raise Exception("Containers and widgets must have a container or "
                    "a toplevel specified as the parent when creating the "
                    "widget.")
        if self.parent is not None and self.category == TOPLEVEL:
            raise Exception("Toplevels must not have a parent specified (or "
                    "the parent must be a connection).")
        if self.category != TOPLEVEL and self.parent.category == WIDGET:
            raise Exception("Containers and widgets can only be added to "
                    "toplevels and containers, not other widgets.")
        (self.widget_schema, self.layout_schema, self.state_schema,
        self.call_schema, self.event_schema) = self._schema[3:8]
        for key, (name, doc, writable, default) in self.widget_schema.items():
            if default is None and key not in kwargs:
                raise Exception("Widget property " + key + " is required to "
                        "construct a widget of type " + type + ", but this "
                        "property was unspecified.")
        if self.parent is not None:
            self.parent.validate_layout_attributes(kwargs)
        # At this point we've validated everything, so we can do ahead and
        # start setting stuff up.
        self.connection.widgets[self.id] = self
        self.widget_properties = dict([(k, kwargs.get(k, d)) 
                for k, (n, doc, w, d) in self.widget_schema.items()])
        if self.parent is not None:
            self.layout_properties = dict([(k, kwargs.get(k, d)) 
                    for k, (n, doc, w, d) in self.parent.layout_schema.items()])
        else:
            self.layout_properties = {}
        self.state_properties = dict([(k, d)
                for k, (n, doc, d,) in self.state_schema.items()])
        self.state_events = dict([(k, Event()) for k in self.state_schema.keys()])
        self.events = dict([(k, Event()) for k in self.event_schema.keys()])
        for k in self.events:
            if k in kwargs:
                self.events[k].listen(kwargs[k])
        self.owner.add_child(self)
        self._resident_ready = True
    
    def help(self):
        """
        Shows help for this widget. This is quite similar to Python's built-in
        help function, but the format is specific to RTK widgets.
        """
        show_help(self._schema, True, self.parent._schema
                if self.parent is not None else None)
    
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
        for key, (name, doc, writable, default) in self.layout_schema.items():
            if default is None and key not in attributes:
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
    def _get_widget(self, name):
        return self.widget_properties[name]
    
    @locked
    def _get_layout(self, name):
        return self.layout_properties[name]
    
    @locked
    def _get_state(self, name):
        return self.state_properties[name]
    
    @locked
    def _get_call(self, name):
        return ResidentCall(self, name)
    
    @locked
    def _get_event(self, name):
        return self.events[name]
    
    @locked
    def _get_state_event(self, name):
        """
        Name should be the name of the state property without "_changed" on
        the end
        """
        return self.state_events[name]
    
    @locked
    def __getattr__(self, name):
        if not object.__getattribute__(self, "_resident_ready"):
            return object.__getattribute__(self, name)
        if name in self.widget_schema:
            return self._get_widget(name)
        elif self.parent is not None and name in self.parent.layout_schema:
            return self._get_layout(name)
        elif name in self.state_schema:
            return self._get_state(name)
        elif name in self.call_schema:
            return self._get_call(name)
        elif name in self.event_schema:
            return self._get_event(name)
        elif (name.endswith("_changed") and
                name[:-len("_changed")] in self.state_schema):
            return self._get_state_event(name[:-len("_changed")])
        # We don't need to delegate to object since __getattr__ is only
        # called if Python can't find the attribute any other way
        raise AttributeError("Widget of type " + self.type + 
                " has no property " + name)
    
    @locked
    def _set_widget(self, name, value):
        if not self.widget_schema[name].writable:
            raise Exception("You can't modify the widget property " + name
                    + " on a widget of type " + self.type + ". This property "
                    "can only be specified when the widget is created.")
        self.widget_properties[name] = value
        self.send_set_widget(name)
    
    @locked
    def _set_layout(self, name, value):
        if not self.parent.layout_schema[name].writable:
            raise Exception("You can't modify the layout property " + name
                    + " on a widget of type " + self.type + " contained "
                    " in a parent of type " + self.parent.type + ". This "
                    "property can only be specified when the widget is created.")
        self.layout_properties[name] = value
        self.parent.send_set_layout(self, name)
    
    @locked
    def _set_state(self, name, value):
        raise Exception("You can't modify the state property " + name
                + " on a widget of type " + self.type + ". State "
                "properties can only be modified by the client.")
    
    @locked
    def _set_call(self, name, value):
        raise Exception("You can't set values for properties "
                "corresponding to calls on widgets. You just tried to "
                "set the property " + name + " on a widget of type "
                + self.type + ".")
    
    @locked
    def _set_event(self, name, value):
        raise Exception("Events can't be set for now. In the future, this "
                "will be allowed, with the result that all listeners on "
                "the specified event will be removed and replaced with "
                "the value you're assigning to this property. You just "
                "tried to set the value for the event " + name + " on a "
                "wiget of type " + self.type + ".")
    
    @locked
    def _set_state_event(self, name, value):
        """
        Name should be the name of the state property without "_changed" on
        the end
        """
        raise Exception("State events can't be set for now. In the future, this "
                "will be allowed, with the result that all listeners on "
                "the specified state event will be removed and replaced with "
                "the value you're assigning to this property. You just "
                "tried to set the value for the state event " + name + "_changed "
                "on a widget of type " + self.type + ".")
    
    @locked
    def __setattr__(self, name, value):
        if not object.__getattribute__(self, "_resident_ready"):
            object.__setattr__(self, name, value)
            return
        if name in self.widget_schema:
            self._set_widget(name, value)
        elif self.parent is not None and name in self.parent.layout_schema:
            self._set_layout(name, value)
        elif name in self.state_schema:
            raise Exception("You can't modify the state property " + name
                    + " on a widget of type " + self.type + ". State "
                    "attributes can only be modified by the client.")
        elif name in self.call_schema:
            self._set_call(name, value)
        elif name in self.event_schema:
            self._set_event(name, value)
        elif (name.endswith("_changed") and
                name[:-len("_changed")] in self.state_schema):
            self._set_state_event(name[:-len("_changed")], value)
        else:
            object.__setattr__(self, name, value)
    
    @locked
    def send_set_widget(self, name):
        self.connection.send({"action": "set_widget", "id": self.id, "properties":
                {name: getattr(self, name)}})
    
    @locked
    def send_set_layout(self, child, name):
        self.connection.send({"action": "set_layout", "id": child.id, "properties":
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
        return self.children[item]
    
    def __len__(self):
        return len(self.children)
    
    def __nonzero__(self):
        return True
    
    def __str__(self):
        if self.parent is not None:
            in_string = (" with parent " + self.parent.type + " "
                    + str(self.parent.id))
        else:
            in_string = "" 
        return "<" + self.type + " instance " + str(self.id) + in_string + ">"
    
    __repr__ = __str__

class ResidentCall(object):
    def __init__(self, widget, name):
        self.widget = widget
        self.name = name
    
    def __call__(self, *args):
        self.widget.connection.send({"action": "call", "id": self.widget.id,
                "name": self.name, "args": args})
    
    def __repr__(self):
        return ("<Call " + self.widget.type + "." + self.name + " on " +
                str(self.widget.id) + ">")
    
    __str__ = __repr__


def gen_attribute_doc(attribute):
    """
    Generates documentation for a single attribute on a widget. attribute
    should be an instance of WidgetPropertySchema, LayoutPropertySchema,
    StatePropertySchema, CallSchema, or EventSchema, in
    librtk.schema.structure.
    """
    return (text_doc.bold(attribute.name) + "\n" + 
            text_doc.indent(linewrap(attribute.doc), "    "))

def gen_group_doc(map, title, add_suffix=True):
    """
    Generates documentation for a particular group of attributes on a widget.
    The group should be specified as a dictionary, and is typically the
    value of one of the attributes on a WidgetSchema instance such as
    widget_properties. The title defines what we're talking about; for
    example, it could be "Widget properties".
    
    Normally, the title will have " defined here:" automatically appended to
    the end of it. Specifying add_suffix=False will disable this.
    
    If the specified map is empty, the empty string will be returned.
    """
    if len(map) == 0:
        return ""
    doc = title
    if add_suffix:
        doc += " defined here:"
    doc += "\n\n"
    keys = list(map.keys())
    keys.sort()
    doc += "\n\n".join(gen_attribute_doc(map[key]) for key in keys)
    return doc

def gen_widget_doc(schema, instance=False, parent_schema=None):
    """
    Generates documentation for the specified widget schema. If instance is
    False, the widget's layout properties will be included under a section
    titled "Layout properties defined here". If instance is True, the
    widget's layout properties will not be included; the specified parent's
    layout properties will be included instead, and they will be included
    under a section titled "Layout properties specified by parent container
    ParentType", where ParentType is the type of the parent and "container"
    is "toplevel" instead if the parent is a toplevel.
    """
    doc = schema.type + " " + text_doc.bold(schema.name) + "\n"
    child_doc = linewrap(schema.doc, 74)
    group_docs = [
            gen_group_doc(schema.widget_properties, "Widget properties"),
            (
                None
                if instance and parent_schema is None else
                gen_group_doc(parent_schema.layout_properties,
                        "Layout properties specified by parent " +
                        parent_schema.type + " " + parent_schema.name + ":",
                        add_suffix=False)
                if instance else
                gen_group_doc(schema.layout_properties, "Layout properties")
            ),
            gen_group_doc(schema.state_properties, "State properties"),
            gen_group_doc(schema.calls, "Calls"),
            gen_group_doc(schema.events, "Events")]
    group_docs = filter(None, group_docs)
    if group_docs:
        child_doc += "\n\n"
        child_doc += ("\n\n" + "-" * 70 + "\n").join(group_docs)
    doc += text_doc.indent(child_doc, " |  ")
    return doc

def show_help(schema, instance=False, parent_schema=None):
    pydoc.pager(gen_widget_doc(schema, instance, parent_schema))

def linewrap(text, width=70):
    return "\n\n".join([textwrap.fill(x, width) for x in text.split("\n\n")])

class ResidentWidgetConstructor(object):
    def __init__(self, schema, type, default_parent=None):
        self.schema = schema
        self.type = type
        self.default_parent = default_parent
    
    def help(self):
        show_help(self.schema)
    
    def __call__(self, parent=None, **kwargs):
        if parent is not None and self.default_parent is not None:
            raise Exception("Can't specify explicit parent for a constructor "
                    "with a default parent specified")
        if parent is None:
            parent = self.default_parent
        if parent is None:
            raise Exception("Need to specify the parent to create this "
                    "widget under, since it's not a toplevel")
        return ResidentWidget(self.type, parent, **kwargs)
    
    def __str__(self):
        return "<" + self.type + " constructor>"
    
    __repr__ = __str__


def default_validator(features, schema, connection):
    schema.update(default_widget_schema.schema)

def listen(port=6785, localhost_only=True, handshake_timeout=10):
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
    server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server.bind(("127.0.0.1" if localhost_only else "", port))
    server.listen(1)
    client, _ = server.accept()
    server.close()
    protocol = ThreadedProtocol(client)
    queue = Queue()
    def connected(connection):
        queue.put(None)
    connection = Connection(protocol, connected)
    connection.start()
    try:
        queue.get(block=True, timeout=handshake_timeout)
    except Empty:
        try:
            connection.close()
        except:
            pass
        raise Exception("Handshake not completed within the specified number "
                "of seconds (which was " + str(handshake_timeout) + ")")
    return connection







































