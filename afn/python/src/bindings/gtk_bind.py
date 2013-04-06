
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

class _GetSetConnectValue(bind.PyValueMixin, bind.Bindable):
    # Takes a function used to get the current value, a function used to modify
    # the value, and the name of a signal that will fire when the value is
    # changed for us (or None for write-only properties)
    def __init__(self, widget, getter, setter, signal):
        self.widget = widget
        self.getter = getter
        self.setter = setter
        self.last_value = getter()
        self.handler = None
        self.handler = widget.connect(signal, self.handle_signal)
    
    def get_value(self):
        return self.last_value
    
    def handle_signal(self, *args):
        new_value = self.getter()
        try:
            self.binder.notify_change(bind.SetValue(new_value))
            self.last_value = new_value
        except: # Validation error
            with BlockHandler(self.widget, self.handler):
                self.setter(self.last_value)
    
    def perform_change(self, change):
        old = self.last_value
        with BlockHandler(self.widget, self.handler):
            self.setter(change.value)
            self.last_value = change.value
        def undo():
            with BlockHandler(self.widget, self.handler):
                self.button.widget.set_state(old)
                self.last_value = old
        return undo
            

class DCheckButton(object):
    def __init__(self):
        self.widget = gtk.CheckButton("")
        self.label = _GetSetConnectValue(self.widget, self.widget.get_label, self.widget.set_label, "notify::label")
        self.active = _GetSetConnectValue(self.widget, self.widget.get_active, self.widget.set_active, "toggled")

    