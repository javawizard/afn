
from threading import Thread, RLock
from Queue import Queue
from traceback import print_exc
import default_widget_schema
import libautobus

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
    def __init__(self, socket, connect_function, validators=[],
            use_default_widgets=True):
        try:
            self.protocol_init
        except AttributeError:
            raise Exception("Connection cannot be instantiated directly. You "
                    "need to instantiate one of its subclasses. "
                    "ThreadedServer and AsyncoreServer are two such "
                    "subclasses included with librtk. Try "
                    "help(librtk.ThreadedServer) or help(librtk.AsyncoreServer) "
                    "for more information.")
        self.socket = socket
        self.connect_function = connect_function
        self.validators = validators[:]
        if use_default_widgets:
            self.validators.append(default_validator)
        self.children = []
        self.widgets = {}
        self.widget_schema = {}
        self.handshake_finished = False
        self.protocol_init()
    
    def start(self):
        """
        Starts this connection. This should be called after the connection has
        been set up (all validators have been added with add_validator, etc).
        """
        self.protocol_start()
    
    def send(self, packet):
        """
        Used by internal RTK code to send a packet to the client. The packet
        should be a dict ready to be put into json.dumps.
        """
        self.protocol_send(packet)
    
    def add_validator(self, validator):
        """
        Adds a new validator function. TODO: more on this later.
        """
        self.validators.append(validator)
    
    def schedule(self, function):
        """
        Schedules a function to be run as soon as possible. The function will
        be put on the event queue, so no incoming changes will be processed
        while the function is running.
        """
        self.protocol_event_ready(function)
    
    def fatal_error(self, message):
        print "FATAL CONNECTION ERROR: " + message
        self.send({"action": "drop", "text": message})
        self.close()
    
    def protocol_receive(self, data):
        """
        Called by subclasses of Connection when they have new data for
        Connection to process. data should be the return value of json.loads
        or a compatible object.
        """
        if not self.handshake_finished: # Perform handshake
            if not data["action"] == "connect":
                self.fatal_error("First message must be a connect message")
                return
            
    
    def protocol_connection_lost(self):
        """
        Called by subclasses of Connection when the connection has been lost.
        If Connection previously called protocol_close (which subclasses
        themselves implement), the connection is not required (but is allowed)
        to call this function.
        """

class ResidentWidget(object):
    pass

class ResidentWidgetConstructor(object):
    pass
    
def default_validator(features, schema):
    schema.update(default_widget_schema.schema)

class ThreadedServer(Thread):
    pass

class ThreadedConnection(Connection):
    def protocol_init(self):
        self.threaded_out_queue = Queue()
        self.input_thread = libautobus.InputThread(self.socket,
                self.protocol_receive, self.protocol_connection_lost)
        self.output_thread = libautobus.OutputThread(self.socket,
                self.threaded_out_queue.get)
        self.event_thread = EventThread()
    
    def protocol_start(self):
        self.input_thread.start()
        self.output_thread.start()
        self.event_thread.start()
    
    def protocol_send(self, data):
        self.threaded_out_queue.put(data)
    
    def protocol_event_ready(self, function):
        self.event_thread.schedule(function)
    
    def protocol_close(self):
        self.threaded_out_queue.put(None)
