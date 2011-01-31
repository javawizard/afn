
from concurrent import synchronized
from threading import RLock
from functools import partial
from utils import print_on_fail

global_lock = RLock()
locked = synchronized(global_lock)

class Connection(object):
    @locked
    def __init__(self, protocol, event_function, close_function, features,
            widget_constructors={}, error_function=lambda fatal, packet: None):
        """
        Creates a new connection. TODO: document this more later.
        
        WARNING: If you're using librtkinter or any Tkinter UI with this,
        DO NOT pass tk.after_idle as the event function. This will cause
        sporadic deadlocks, which won't be noticeable unless the server sends
        a whole bunch of UI updates all at once. Instead, have event_function
        be the put function of a queue, and then write a function that
        registers itself with tk.after to run every, say, 200ms and runs all
        the events in the queue.
        http://me.opengroove.org/2011/01/threading-locks-and-tkinters-afteridle.html
        (which is a blog post I wrote on this issue) provides more info as to
        why these deadlocks occur.
        """
        self.protocol = protocol
        self.schedule = event_function
        self.close_function = close_function
        self.error_function = error_function
        self.features = features
        self.widget_constructors = widget_constructors.copy()
        self.started = False
        self.handshake_finished = False
        self.closed = False
        self.widget_map = {}
        self.toplevels = []
        # We shouldn't need a pre-start list of messages like the server does
        # since the server's not supposed to send us anything until after we
        # send it the handshake packet. I might add such a list later if a lot
        # of non-conformant servers (the type that want to accept every client
        # and so send the accept message immediately after a connect) start
        # turning up on the internet.
        protocol.protocol_init(self)
    
    @locked
    def start(self):
        if self.started:
            raise Exception("You can't call Connection.start twice.")
        self.started = True
        self.protocol.protocol_start()
        self.send({"action": "connect", "features": self.features})
    
    @locked
    def send(self, packet):
        self.protocol.protocol_send(packet)
    
    @locked
    def send_event(self, id, name, user, *args):
        """
        Sends an event to the server. id is the id of the widget that the
        event occurred on. name is the name of the event. user is whether or
        not the event was triggered by a user interacting with the interface
        (as opposed to a widget function being called). *args are the arguments
        to be sent with the event.
        """
        self.send({"action":"event", "id":id, "name":name,
                "user":user, "args":args})
    
    @locked
    def send_set_state(self, id, properties):
        """
        """
        self.send({"action":"set_state", "id":id, "properties": properties})
    
    @locked
    def fatal_error(self, message):
        # TODO: Right now, errors that are most likely the fault of the
        # programmer and errors that result from the client messing up are
        # both printed by this function. Perhaps split them out so the
        # application developer knows what to look for to know if they have
        # potential problems with their application.
        print "FATAL CONNECTION ERROR: " + message
        self.send({"action": "drop", "text": message})
        self.close()
    
    @locked
    def protocol_receive(self, packet):
        if not self.started:
            print ("HUGE MAJOR PROBLEM: Packet received before handshake "
                    "sent; this packet is being ignored, which is most "
                    "likely going to result in a corrupted datastream.")
            return
        if self.closed:
            return
        if packet["action"] == "error":
            print "SERVER REPORTED AN ERROR: " + packet["text"]
            self.error_function(False, packet)
            return
        if packet["action"] == "drop":
            print "SERVER REPORTED A FATAL ERROR: " + packet["text"]
            self.error_function(True, packet)
            self.close()
            return
        if not self.handshake_finished:
            if packet["action"] != "accept":
                self.fatal_error("First message must be an accept message")
                return
            self.server_features = packet["features"]
            self.handshake_finished = True
            return
        action = packet["action"]
        if action == "create":#id,parent,type,index,p_widget,p_layout
            self.schedule(partial(self.on_create, packet["id"],
                    packet.get("parent", None), packet["type"],
                    packet.get("index", None), packet.get("p_widget", {}),
                    packet.get("p_layout", {})))
        elif action == "destroy":
            self.schedule(partial(self.on_destroy, packet["id"]))
        elif action == "reorder":
            self.schedule(partial(self.on_reorder, packet))
        elif action == "set_widget":
            self.schedule(partial(self.on_set_widget, packet["id"],
                    packet["properties"]))
        elif action == "set_layout":
            self.schedule(partial(self.on_set_layout, packet["id"],
                    packet["properties"]))
        elif action == "call":
            self.schedule(partial(self.on_call, packet["id"], packet["name"],
                    packet["args"]))
        else:
            print "Ignoring message with action " + action
    
    @locked
    def protocol_connection_lost(self):
        self.close()
    
    def tri_call(self, widget, name, *args):
        if widget.parent and hasattr(widget.parent, "pre_" + name):
            getattr(widget.parent, "pre_" + name)(widget, *args)
        if hasattr(widget, name):
            getattr(widget, name)(*args)
        if widget.parent and hasattr(widget.parent, "post_" + name):
            getattr(widget.parent, "post_" + name)(widget, *args)
    
    @locked
    def close(self):
        if self.closed:
            return
        self.closed = True
        self.protocol.protocol_close()
        self.schedule(partial(self.on_close))
    
    @print_on_fail
    def on_create(self, id, parent_id, type, index, widget_properties,
            layout_properties):
        if type not in self.widget_constructors:
            self.fatal_error("Unsupported widget type: " + type)
            return
        if parent_id is not None and parent_id not in self.widget_map:
            self.fatal_error("Trying to add child " + str(id) + " to parent "
                    + str(parent_id) + ", which does not exist.")
            return
        if id in self.widget_map:
            self.fatal_error("Trying to add a widget with id " + str(id) + 
                    " but there's already a non-destroyed widget with that id")
            return
        constructor = self.widget_constructors[type]
        parent_widget = self.widget_map[parent_id] if parent_id is not None else None
        widget = constructor(self, id, parent_widget, type, index,
                widget_properties.copy(), layout_properties.copy())
        self.widget_map[id] = widget
        if parent_widget is None:
            self.toplevels.append(widget)
        else:
            parent_widget.children[index:index] = [widget]
        self.tri_call(widget, "setup")
    
    @print_on_fail
    def on_destroy(self, id):
        widget = self.widget_map[id]
        for child in widget.children[:]:
            self.on_destroy(child.id)
        self.tri_call(widget, "destroy")
        del self.widget_map[id]
        if widget.parent:
            widget.parent.children.remove(widget)
            widget.parent = None # Unlink to try and minimize reference cycles
        else:
            self.toplevels.remove(widget)
    
    @print_on_fail
    def on_set_widget(self, id, properties):
        widget = self.widget_map[id]
        widget.widget_properties.update(properties)
        widget.update_widget(properties.copy())
    
    @print_on_fail
    def on_set_layout(self, id, properties):
        widget = self.widget_map[id]
        widget.layout_properties.update(properties)
        if widget.parent:
            widget.parent.update_layout(widget, properties.copy())
    
    @print_on_fail
    def on_call(self, id, name, args):
        widget = self.widget_map[id]
        widget.call(name, args)
    
    @print_on_fail
    def on_close(self):
        for toplevel in self.toplevels[:]:
            self.on_destroy(toplevel.id)
        self.close_function()
    

class Widget(object):
    def __init__(self, connection, id, parent, type, index,
            widget_properties, layout_properties):
        self.connection = connection
        self.id = id
        self.parent = parent
        self.type = type
        self.index = index
        self.widget_properties = widget_properties
        self.layout_properties = layout_properties
        self.children = []
    
    def send_event(self, name, user, *args):
        self.connection.send_event(self.id, name, user, *args)
    
    def send_set_state(self, properties={}, **kwargs):
        p = {}
        p.update(properties)
        p.update(kwargs)
        self.connection.send_set_state(self.id, p)
    
    def call(self, name, args):
        self.connection.fatal_error("Widget " + str(self.id) + " of type "
                + self.type + " does not provide any calls, but the server "
                "just tried to perform a call.")

#class Dispatcher(object):
#    """
#    This class specifies the dispatcher interface. Dispatchers are objects
#    passed into Connection instances. They implement the actual display
#    function of an RTK client.
#    
#    Dispatchers are not required to subclass from this class, but they can;
#    This class exists primarily to document the functions a dispatcher must
#    have. All functions on this class raise exceptions indicating that they
#    have not been implemented.
#    """
#    def dispatcher_init(self, connection):
#        """
#        Initializes this dispatcher. The connection this dispatcher was passed
#        to is passed as the argument.
#        """
#        raise Exception("Not implemented")
#    
#    def dispatcher_ready(self):
#        """
#        Called once the connection has been started and the handshake has been
#        completed. The dispatcher should expect widget function calls to start
#        arriving shortly after this function is called. No other functions
#        besides dispatcher_init and dispatcher_close will be called before
#        this function is called.
#        """
#        raise Exception("Not implemented")
#    
#    def dispatcher_close(self):
#        """
#        Called when the connection has been shut down. The dispatcher should
#        destroy all widgets that have not yet been destroyed. This will be the
#        last function called on the dispatcher.
#        """
#        raise Exception("Not implemented")
#    
#    def dispatcher_received_error(self, error):
#        """
#        Called when an error is sent by the server. This is not a fatal error;
#        communications will continue as normal after the error. The dispatcher
#        should inform the user of the error, which is passed into this
#        function as a dictionary with one key, text, which is the text of the
#        error.
#        """
#        raise Exception("Not implemented")
#    
#    def dispatcher_received_drop(self, error):
#        """
#        Same as dispatcher_received_error, but the error is fatal;
#        dispatcher_close will be called shortly after this function returns. 
#        """
#        raise Exception("Not implemented")
#    
#    def dispatcher_create(self, id, parent, type, index, widget_properties,
#            layout_properties):
#        """
#        Called to create a widget. id is the integer id that is to be assigned
#        to the widget. parent is the id of the parent widget, or None if this
#        is to be a toplevel. type is the type of the widget ("Window", "VBox",
#        "Button", "Label", etc). index is the index within the widget's parent
#        at which the widget is to be created (which will range from 0 to the
#        number of widgets the parent currently contains, inclusive); this is
#        unspecified in the case of a toplevel. widget_properties is a
#        dictionary of the properties the widget is to be created with.
#        layout_properties is a dictionary of the layout properties the widget
#        is to be created with.
#        """
#        raise Exception("Not implemented")
#    
#    def dispatcher_destroy(self, id):
#        """
#        Called to destroy a widget. id is the integer id of the widget to
#        destroy.
#        """
#        raise Exception("Not implemented")
#    
#    def dispatcher_set_widget(self, id, properties):
#        """
#        Called to change a widget's widget properties. id is the integer id of
#        the widget. properties is a dictionary of properties to change.
#        """
#        raise Exception("Not implemented")
#    
#    def dispatcher_set_layout(self, id, properties):
#        """
#        Same as dispatcher_set_widget, but sets layout properties instead of
#        widget properties.
#        """
#        raise Exception("Not implemented")
    

































