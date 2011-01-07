
from concurrent import synchronized
from threading import RLock

global_lock = RLock()
locked = synchronized(global_lock)

class Connection(object):
    @locked
    def __init__(self, protocol, dispatcher, features):
        self.protocol = protocol
        self.dispatcher = dispatcher
        self.features = features
        self.started = False
        self.handshake_finished = False
        self.closed = False
        # We shouldn't need a pre-start list of messages like the server does
        # since the server's not supposed to send us anything until after we
        # send it the handshake packet. I might add such a list later if a lot
        # of non-conformant servers (the type that want to accept every client
        # and so send the accept message immediately after a connect) start
        # turning up on the internet.
        if self.started:
            raise Exception("You can't call Connection.start twice.")
        protocol.protocol_init(self)
        dispatcher.dispatcher_init(self)
    
    @locked
    def start(self):
        self.started = True
        self.dispatcher.dispatcher_start()
        self.protocol.protocol_start()
        self.send({"action": "connect", "features": self.features})
    
    @locked
    def send(self, packet):
        self.protocol.protocol_send(packet)
    
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
            self.dispatcher.dispatcher_received_error(packet)
            return
        if packet["action"] == "drop":
            print "SERVER REPORTED A FATAL ERROR: " + packet["text"]
            self.dispatcher.dispatcher_received_drop(packet)
            self.close()
            return
        if not self.handshake_finished:
            if packet["action"] != "accept":
                self.fatal_error("First message must be an accept message")
                return
            self.server_features = packet["features"]
            self.handshake_finished = True
            self.dispatcher.dispatcher_ready()
        action = packet["action"]
        if action == "create":#i,parent,typein,dex,p_widget,p_layout
            self.dispatcher.dispatcher_create(packet["id"],
                    packet.get("parent", None), packet["type"],
                    packet.get("index", None), packet.get("p_widget", {}),
                    packet.get("p_layout", {}))
        elif action == "destroy":
            self.dispatcher.dispatcher_destroy(packet["id"])
        elif action == "reorder":
            self.dispatcher.dispatcher_reorder(packet)
        elif action == "set_widget":
            self.dispatcher.dispatcher_set_widget(packet["id"],
                    packet["properties"])
        elif action == "set_layout":
            self.dispatcher.dispatcher_set_layout(packet["id"],
                    packet["properties"])
        elif action == "call":
            self.dispatcher.dispatcher_call(packet)
        else:
            print "Ignoring message with action " + action
    
    @locked
    def protocol_connection_lost(self):
        self.close()
    
    @locked
    def close(self):
        if self.closed:
            return
        self.closed = True
        self.protocol.protocol_close()
        self.dispatcher.dispatcher_close()

class Dispatcher(object):
    """
    This class specifies the dispatcher interface. Dispatchers are objects
    passed into Connection instances. They implement the actual display
    function of an RTK client.
    
    Dispatchers are not required to subclass from this class, but they can;
    This class exists primarily to document the functions a dispatcher must
    have. All functions on this class raise exceptions indicating that they
    have not been implemented.
    """
    def dispatcher_init(self, connection):
        """
        Initializes this dispatcher. The connection this dispatcher was passed
        to is passed as the argument.
        """
        raise Exception("Not implemented")
    
    def dispatcher_ready(self):
        """
        Called once the connection has been started and the handshake has been
        completed. The dispatcher should expect widget function calls to start
        arriving shortly after this function is called. No other functions
        besides dispatcher_init and dispatcher_close will be called before
        this function is called.
        """
    
    def dispatcher_close(self):
        """
        Called when the connection has been shut down. The dispatcher should
        destroy all widgets that have not yet been destroyed. This will be the
        last function called on the dispatcher.
        """
    
    def dispatcher_received_error(self, error):
        """
        Called when an error is sent by the server. This is not a fatal error;
        communications will continue as normal after the error. The dispatcher
        should inform the user of the error, which is passed into this
        function as a dictionary with one key, text, which is the text of the
        error.
        """
    
    def dispatcher_received_drop(self, error):
        """
        Same as dispatcher_received_error, but the error is fatal;
        dispatcher_close will be called shortly after this function returns. 
        """
    
    def dispatcher_create(self, id, parent, type, index, widget_properties,
            layout_properties):
        """
        Called to create a widget. id is the integer id that is to be assigned
        to the widget. parent is the id of the parent widget, or None if this
        is to be a toplevel. type is the type of the widget ("Window", "VBox",
        "Button", "Label", etc). index is the index within the widget's parent
        at which the widget is to be created (which will range from 0 to the
        number of widgets the parent currently contains, inclusive); this is
        unspecified in the case of a toplevel. widget_properties is a
        dictionary of the properties the widget is to be created with.
        layout_properties is a dictionary of the layout properties the widget
        is to be created with.
        """
    
    def dispatcher_destroy(self, id):
        """
        Called to destroy a widget. id is the integer id of the widget to
        destroy.
        """
    
    def dispatcher_set_widget(self, id, properties):
        """
        Called to change a widget's widget properties. id is the integer id of
        the widget. properties is a dictionary of properties to change.
        """
    
    def dispatcher_set_layout(self, id, properties):
        """
        Same as dispatcher_set_widget, but sets layout properties instead of
        widget properties.
        """
    

































