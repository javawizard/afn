
import gtk
from bindings import bind

class BlockHandler(object):
    def __init__(self, widget, handler):
        self.widget = widget
        self.handler = handler
    
    def __enter__(self):
        self.widget.handler_block(self.handler)
    
    def __exit__(self, *args):
        self.widget.handler_unblock(self.handler)

class _PropertyValue(bind.PyValueMixin, bind.Bindable):
    # Takes a function used to get the current value, a function used to modify
    # the value, and the name of a signal that will fire when the value is
    # changed for us (or None for write-only properties)
    def __init__(self, widget, property):
        self.widget = widget
        self.property = property
        self.last_value = widget.get_property(property)
        self.handler = widget.connect("notify::" + property, self.handle_signal)
    
    def get_value(self):
        return self.last_value
    
    def handle_signal(self, *args):
        # TODO: Make sure we're updating last_value here properly, and same
        # with perform_change
        new_value = self.widget.get_property(self.property)
        try:
            self.binder.notify_change(bind.SetValue(new_value))
            self.last_value = new_value
        except: # Validation error
            with BlockHandler(self.widget, self.handler):
                self.widget.set_property(self.property, self.last_value)
    
    def perform_change(self, change):
        old = self.last_value
        with BlockHandler(self.widget, self.handler):
            self.set_property(self.property, change.value)
            self.last_value = change.value
        def undo():
            with BlockHandler(self.widget, self.handler):
                self.set_property(self.property, old)
                self.last_value = old
        return undo
            

class DCheckButton(object):
    def __init__(self):
        self.widget = gtk.CheckButton("")
        self.label = _PropertyValue(self.widget, "label")
        self.active = _PropertyValue(self.widget, "active")

    