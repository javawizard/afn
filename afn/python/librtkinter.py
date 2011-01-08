
import Tkinter as tkinter
from librtkclient import Connection, Dispatcher
from functools import partial

__doc__ = """\
This module is an RTK viewer library that uses Tkinter to show the
application.

The simplest way to start a viewer displaying an RTK application located at
localhost port 6785 would be something like this:

from librtk import ThreadedProtocol
from librtkclient import Connection
from librtkinter import TkinterDispatcher, default_features
from socket import socket
s = socket()
s.connect(("localhost", 6785))
Connection(ThreadedProtocol(socket), TkinterDispatcher(), default_features).start()
"""

class TkinterDispatcher(Dispatcher):
    """
    This is the Tkinter dispatcher implementation.
    """
    def __init__(self, event_function, close_function, widget_constructors,
            use_default_widgets=True):
        """
        Creates a new Tkinter dispatcher. event_function is a function that
        will be called whenever this class needs a function run on the Tkinter
        event loop. close_function is a function that will be called when the
        dispatcher is informed that the connection has been lost.
        widget_constructors is a map of widget names to functions that can be
        used to construct instances of those widgets. use_default_widgets
        can be true to automatically add the global default_widgets to the
        dictionary; widgets specified in widget_constructors will override
        those specified in default_widgets.
        """
        self.schedule = event_function
        self.widget_constructors = {}
        # Update with defaults first, then user-specified so that
        # user-specified constructors override the buit-in ones
        if self.use_default_widgets:
            self.widget_constructors.update(default_widgets)
        self.widget_constructors.update(widget_constructors)
        self.widget_map = {} # Map of widget ids to widgets
        self.toplevels = [] # List of toplevel widget objects
    
    def fatal_error(self, text):
        self.connection.fatal_error(text)

    def dispatcher_init(self, connection):
        self.connection = connection

    def dispatcher_ready(self):
        pass

    def dispatcher_close(self):
        print "Tkinter dispatcher closed"
        print "FIXME: actually destroy windows etc here"

    def dispatcher_received_error(self, error):
        self.schedule(partial(self.received_error, error))

    def dispatcher_received_drop(self, error):
        self.schedule(partial(self.received_drop, error))

    def dispatcher_create(self, id, parent, type, index, widget_properties,
            layout_properties):
        self.schedule(partial(self.create, id, parent, type, index,
                widget_properties, layout_properties))

    def dispatcher_destroy(self, id):
        self.schedule(partial(self.destroy, id))

    def dispatcher_set_widget(self, id, properties):
        self.schedule(partial(self.set_widget, id, properties))

    def dispatcher_set_layout(self, id, properties):
        self.schedule(partial(self.set_layout, id, properties))
    
    def received_error(self, error):
        print "ERROR RECEIVED: " + error["text"]
    
    def received_drop(self, error):
        print "FATAL ERROR RECEIVED: " + error["text"]
    
    def create(self, id, parent, type, index, widget_properties,
            layout_properties):
        if type not in self.widget_constructors:
            self.fatal_error("Unsupported widget type: " + type)
            return
        if parent is not None and parent not in self.widget_map:
            self.fatal_error("Trying to add child " + str(id) + " to parent "
                    + str(parent) + ", which does not exist.")
            return
        constructor = self.widget_constructors[type]
        widget = constructor()
        widget.id = id
        widget.parent = self.widget_map[parent] if parent is not None else None
        widget.type = type
        widget.connection = self.connection
        widget.children = []
        widget.widget_properties = widget_properties.copy()
        widget.layout_properties = layout_properties.copy()
        widget.state_properties = {}
        if parent is None:
            self.toplevels.append(widget)
        else:
            widget.parent.children[index:index] = [widget]
        widget.setup()
        if parent is not None:
            widget.parent.setup_child(widget)
    
    def set_widget(self, id, properties):
        print "FIXME: implement TkinterDispatcher.set_widget"

    
    

























