
import gtk
from bindings import bind

class _DCheckButtonActive(bind.PyValueMixin, bind.Bindable):
    def __init__(self, button):
        self.button = button
        self.last_value = self.button.widget.get_active()
        self.toggled_handler = self.button.widget.connect("toggled", self.on_toggled)
    
    def get_value(self):
        return self.last_value
    
    def on_toggled(self, button):
        new_state = button.widget.get_active()
        try:
            self.binder.notify_change(bind.SetValue(new_state))
            self.last_value = new_state
        except:
            try:
                self.button.widget.handler_block(self.toggled_handler)
                self.button.widget.set_state(self.last_value)
            finally:
                self.button.widget.handler_unblock(self.toggled_handler)
    
    def perform_change(self, change):
        old = self.last_value
        try:
            self.button.widget.handler_block(self.toggled_handler)
            self.button.widget.set_state(change.value)
        finally:
            self.button.widget.handler_unblock(self.toggled_handler)
        def undo():
            try:
                self.button.widget.handler_block(self.toggled_handler)
                self.button.widget.set_state(old)
            finally:
                self.button.widget.handler_unblock(self.toggled_handler)
        return undo
            

class _DCheckButtonLabel(bind.PyValueMixin, bind.Bindable):
    def __init__(self, button):
        self.button = button

class DCheckButton(object):
    def __init__(self):
        self.widget = gtk.CheckButton("")
        self.label = _DCheckButtonLabel(self)
        self.active = _DCheckButtonActive(self)

    