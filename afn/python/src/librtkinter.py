
try:
    import Tix as tkinter
    tix_support = True
except ImportError:
    tix_support = False
    import Tkinter as tkinter
import librtkclient
from Queue import Queue, Empty
from traceback import print_exc
from utils import filter_dict
import tkFont

"""
This module is an RTK viewer library that uses Tkinter to show the
application.

The simplest way to start a viewer displaying an RTK application located at
localhost port 6785 would be something like this:

import librtkinter
from librtk.protocols import ThreadedProtocol
from socket import socket
s = socket()
s.connect(("localhost", 6785))
connection, tk_master = librtkinter.start_connection(ThreadedProtocol(s))
tk_master.mainloop()
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
    tk_use_font = False
    
    def post_setup(self, child):
        # This will always be overridden to pack the widget in its parent,
        # so we'll use it to send back an error.
        self.connection.fatal_error("Widget type " + type(self)
                + " is not a container, but you just tried to add a child "
                "to it.")
    
    def destroy(self):
        self.widget.destroy()
    
    def update_widget(self, properties=None):
        """
        Default implementation of update_widget. This looks for a field on the
        widget class named tk_fields. If it's present, it should be a list of
        fields, with each field represented by a list containing either two
        or three items: [rtk_field_name, tkinter_field_name, default]. The
        default, if present, will be substituted for the value to set on the
        field if bool(value_to_be_set) == False.
        
        This also sets up font information if the widget class has a field
        tk_use_font = True.
        """
        if hasattr(self, "tk_fields"):
            for field in self.tk_fields:
                # Get the name of the field and its corresponding TK name
                rtk_name, tkinter_name = field[0:2]
                # Now we se if this request is supposed to change this field
                if rtk_name in properties:
                    # Looks like it is. We'll set the field's value if it has
                    # a true value or if there's no default to look to in case
                    # the specified value is a false value.
                    if properties[rtk_name] or len(field) <= 2:
                        # Yep, it's true! We'll set the value and we're done.
                        self.widget[tkinter_name] = properties[rtk_name]
                    else: # The value we're supposed to set this field to is
                        # a false value and the field has a default. We'll set
                        # the field to the default value.
                        self.widget[tkinter_name] = field[2]
        if self.tk_use_font:
            if "font_size" in properties:
                self.update_font()
    
    def update_font(self):
        font_info = filter_dict(self.widget_properties, {"font_size": "size"})
        font_info = dict([(k, v) for k, v in font_info.items() if v])
        font = tkFont.Font(**font_info)
        self.widget["font"] = font
    
    def update_layout(self, child, properties):
        pass
    
    @property
    def container(self):
        return self.widget


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
    tk_use_font = True
    
    def setup(self):
        self.widget = tkinter.Label(self.parent.container,
                text=self.widget_properties["text"])
        self.default_background = self.widget["bg"]
        if self.widget_properties["background"]:
            self.widget["bg"] = self.widget_properties["background"]
        if (self.widget_properties["font_size"] or
                self.widget_properties["font_family"]):
            self.update_font()
    
    def update_widget(self, properties):
        Widget.update_widget(self, properties)
        if "background" in properties:
            if properties["background"]:
                self.widget["bg"] = properties["background"]
            else:
                self.widget["bg"] = self.default_background

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
    tk_fields = [["width", "width"], ["background", "bg", "#ffffff"]]
    tk_use_font = True
    
    def setup(self):
        self.var = tkinter.StringVar(self.connection.tk_master, "")
        self.widget = tkinter.Entry(self.parent.container,
                width=self.widget_properties["width"], textvariable=self.var,
                bg=self.widget_properties["background"] or "#ffffff")
        self.var.trace_variable("w", self.text_changed)
        self.send_set_state(text="")
        self.user_set = False
        if (self.widget_properties["font_size"] or
                self.widget_properties["font_family"]):
            self.update_font()
    
    def text_changed(self, *args):
        text = self.var.get()
        # FIXME: include user_set as the _user argument, or add something to
        # librtkclient to do this
        self.send_set_state(text=text)
        self.user_set = False
    
    def call(self, name, args):
        if name == "set_text":
            text, = args
            self.user_set = True
            self.var.set(text)
        else:
            self.connection.fatal_error("Invalid call " + name + " on TextBox")


widget_list = [Window, Label, VBox, HBox, BorderPanel, Button, Table, TextBox]
widget_set = dict([(w.__name__, w) for w in widget_list])
feature_set = ["widget:" + w for w in widget_set.keys()]

def start_connection(protocol):
    """
    Sets up a Tkinter viewer connected to the specified protocol instance. A
    toplevel Tk instance will be created and all the event processing stuff
    set up for it. A connection wrapping it will then be created and started,
    and this function will return a 2-tuple of the connection and the master
    Tk instance. This can be used to implement a simple RTK viewer like so:
    
    import socket, librtkinter
    from librtk.protocols import ThreadedProtocol
    s = socket.connect(("localhost", 6785))
    connection, tk = librtkinter.start_connection(ThreadedProtocol(s))
    tk.mainloop()
    
    
    """
    tk = tkinter.Tk()
    tk.withdraw()
    event_queue = Queue()
    connection = librtkclient.Connection(protocol, event_queue.put, tk.destroy,
            feature_set, widget_set)
    connection.tk_master = tk
    connection.start()
    def idle():
        try:
            event_queue.get(block=False)()
        except Empty:
            tk.after(100, idle)
        except:
            tk.after(100, idle)
            print_exc()
        else:
            tk.after(0, idle)
    tk.after(100, idle)
    return connection, tk


























