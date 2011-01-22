
import Tkinter as tkinter
import librtkclient
from functools import partial
from utils import filter_dict

"""
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

#class TkinterDispatcher(Dispatcher):
#    """
#    This is the Tkinter dispatcher implementation.
#    """
#    def __init__(self, master, event_function, close_function,
#            widget_constructors={}, use_default_widgets=True):
#        """
#        Creates a new Tkinter dispatcher. master is a Tk instance under which
#        widgets and windows will be created. event_function is a function that
#        will be called whenever this class needs a function run on the Tkinter
#        event loop. close_function is a function that will be called when the
#        dispatcher is informed that the connection has been lost.
#        widget_constructors is a map of widget names to functions that can be
#        used to construct instances of those widgets. use_default_widgets
#        can be true to automatically add the global default_widgets to the
#        dictionary; widgets specified in widget_constructors will override
#        those specified in default_widgets.
#        """
#        self.master = master
#        self.schedule = event_function
#        self.close_function = close_function
#        self.widget_constructors = {}
#        # Update with defaults first, then user-specified so that
#        # user-specified constructors override the buit-in ones
#        if use_default_widgets:
#            self.widget_constructors.update(default_widgets)
#        self.widget_constructors.update(widget_constructors)
#        self.widget_map = {} # Map of widget ids to widgets
#        self.toplevels = [] # List of toplevel widget objects
#    
#    def fatal_error(self, text):
#        self.connection.fatal_error(text)
#
#    def dispatcher_init(self, connection):
#        self.connection = connection
#    
#    def dispatcher_start(self):
#        pass
#
#    def dispatcher_ready(self):
#        pass
#
#    def dispatcher_close(self):
#        self.schedule(partial(self.close))
#
#    def dispatcher_received_error(self, error):
#        self.schedule(partial(self.received_error, error))
#
#    def dispatcher_received_drop(self, error):
#        self.schedule(partial(self.received_drop, error))
#
#    def dispatcher_create(self, id, parent, type, index, widget_properties,
#            layout_properties):
#        self.schedule(partial(self.create, id, parent, type, index,
#                widget_properties, layout_properties))
#
#    def dispatcher_destroy(self, id):
#        self.schedule(partial(self.destroy, id))
#
#    def dispatcher_set_widget(self, id, properties):
#        self.schedule(partial(self.set_widget, id, properties))
#
#    def dispatcher_set_layout(self, id, properties):
#        self.schedule(partial(self.set_layout, id, properties))
#    
#    def received_error(self, error):
#        print "ERROR RECEIVED: " + error["text"]
#    
#    def received_drop(self, error):
#        print "FATAL ERROR RECEIVED: " + error["text"]
#    
#    def create(self, id, parent, type, index, widget_properties,
#            layout_properties):
#        if type not in self.widget_constructors:
#            self.fatal_error("Unsupported widget type: " + type)
#            return
#        if parent is not None and parent not in self.widget_map:
#            self.fatal_error("Trying to add child " + str(id) + " to parent "
#                    + str(parent) + ", which does not exist.")
#            return
#        if id in self.widget_map:
#            self.fatal_error("Trying to add a widget with id " + str(id) +
#                    " but there's already a non-destroyed widget with that id")
#        constructor = self.widget_constructors[type]
#        widget = constructor()
#        widget.dispatcher = self
#        widget.id = id
#        widget.parent = self.widget_map[parent] if parent is not None else None
#        widget.type = type
#        widget.connection = self.connection
#        widget.children = []
#        widget.widget_properties = widget_properties.copy()
#        widget.layout_properties = layout_properties.copy()
#        widget.state_properties = {}
#        widget.send_event = partial(self.connection.send_event, id)
#        self.widget_map[id] = widget
#        if parent is None:
#            self.toplevels.append(widget)
#        else:
#            widget.parent.children[index:index] = [widget]
#        widget.setup()
#        if parent is not None:
#            widget.parent.setup_child(widget)
#    
#    def destroy(self, id):
#        widget = self.widget_map[id]
#        # FIXME: Tell the parent about the destroy, too
#        widget.destroy()
#        del self.widget_map[id]
#        # FIXME: remove from the list of toplevels
#    
#    def set_widget(self, id, properties):
#        widget = self.widget_map[id]
#        widget.widget_properties.update(properties)
#        widget.update_widget_properties(properties)
#    
#    def set_layout(self, id, properties):
#        widget = self.widget_map[id]
#        widget.layout_properties.update(properties)
#        if widget.parent is not None:
#            widget.parent.update_layout_properties(widget, properties)
#    
#    def close(self):
#        for toplevel in self.toplevels[:]:
#            toplevel.destroy()
#        self.close_function()
#        # TODO: consider whether the children should be destroyed first


class Widget(librtkclient.Widget):
    def post_setup(self, child):
        # This will always be overridden to pack the widget in its parent,
        # so we'll use it to send back an error.
        self.connection.fatal_error("Widget type " + type(self)
                + " is not a container, but you just tried to add a child "
                "to it.")
    
    def destroy(self):
        self.widget.destroy()
    
    def update_widget(self, properties=None):
        if hasattr(self, "tk_fields"):
            fields = self.tk_fields
            for rtk_name, tkinter_name in fields:
                if rtk_name in properties:
                    self.widget[tkinter_name] = properties[rtk_name]
    
    def update_layout(self, child, properties):
        pass
    
    @property
    def container(self):
        return self.widget
    
    @container.setter
    def container(self, value):
        # Override the property if the container is set
        self.__dict__["container"] = value


class Window(Widget):
    def setup(self):
        self.widget = tkinter.Toplevel(self.connection.tk_master)
        self.widget.protocol("WM_DELETE_WINDOW", self.window_close_request)
        if "title" in self.widget_properties:
            self.widget.title(self.widget_properties["title"])
    
    def post_setup(self, child):
        child.widget.pack(fill=tkinter.BOTH, expand=True)
    
    def update_widget(self, properties):
        if "title" in properties:
            self.widget.title(properties["title"])
    
    def window_close_request(self):
        self.send_event("close_request", True)


class Table(Widget):
    grid_rule_map = {"rowspan": "rowspan", "colspan": "columnspan",
            "row": "row", "col": "column", "pin": "sticky"}
     
    def setup(self):
        self.widget = tkinter.Frame(self.parent.container)
    
    def post_setup(self, child):
        child.widget.grid(**filter_dict(child.layout_properties,
                Table.grid_rule_map))
    
    def update_layout(self, child, properties):
        child.widget.grid(**filter_dict(properties, Table.grid_rule_map))


class Label(Widget):
    tk_fields = [["text", "text"]]
    
    def setup(self):
        self.widget = tkinter.Label(self.parent.container,
                text=self.widget_properties["text"])
    

class VBox(Widget):
    def setup(self):
        self.widget = tkinter.Frame(self.parent.container)
    
    def post_setup(self, child):
        child.widget.pack(side=tkinter.TOP)


class HBox(Widget):
    def setup(self):
        self.widget = tkinter.Frame(self.parent.container)
    
    def post_setup(self, child):
        child.widget.pack(side=tkinter.LEFT)


class BorderPanel(Widget):
    tk_fields = [["border_width", "borderwidth"]]
    
    def setup(self):
        self.widget = tkinter.Frame(self.parent.container)
        self.widget["relief"] = tkinter.SOLID
        self.widget["borderwidth"] = (self.widget_properties["border_width"]
                if "border_width" in self.widget_properties else 1)
    
    def post_setup(self, child):
        child.widget.pack(fill=tkinter.BOTH, expand=True)


class Button(Widget):
    tk_fields = [["text", "text"]]
    
    def setup(self):
        self.widget = tkinter.Button(self.parent.container,
                text=self.widget_properties["text"],
                command=self.button_clicked)
    
    def button_clicked(self):
        self.send_event("clicked", True)


class TextBox(Widget):
    tk_fields = [["width", "width"]]
    
    def setup(self):
        self.widget = tkinter.Entry(self.parent.container,
                width=self.widget_properties["width"])


widget_list = [Window, Label, VBox, HBox, BorderPanel, Button, Table]
widget_set = dict([(w.__name__, w) for w in widget_list])
feature_set = ["widget:" + w for w in widget_set.keys()]

























