
from gi.repository import Gtk as gtk, GObject as gobject
from bindings import bind
from functools import partial

class BlockHandler(object):
    def __init__(self, widget, handler):
        self.widget = widget
        self.handler = handler
    
    def __enter__(self):
        self.widget.handler_block(self.handler)
    
    def __exit__(self, *args):
        self.widget.handler_unblock(self.handler)


class PropertyDict(bind.PyDictMixin, bind.Bindable):
    def __init__(self, widget):
        self.widget = widget
        # Only support primitive values for now
        self.prop_names = [p.name for p in widget.props if p.flags & gobject.PARAM_READABLE]
        self.handlers = {}
        self.last_values = dict((p, self.widget.get_property(p)) for p in self.prop_names)
        self.connect()
    
    def connect(self):
        if self.handlers:
            raise Exception("Already connected")
        for prop_name in self.prop_names:
            self.handlers[prop_name] = self.widget.connect("notify::" + prop_name, partial(self.signal, prop_name))
    
    def disconnect(self):
        if not self.handlers:
            raise Exception("Not connected")
        for handler in self.handlers.values():
            self.widget.disconnect(handler)
    
    def get_value(self):
        return bind.SimpleMapping(self._get_function, lambda: iter(self.prop_names))
    
    def _get_function(self, key):
        if key not in self.prop_names:
            raise KeyError(key)
        return self.widget.get_property(key)
    
    def signal(self, prop_name, *args):
        try:
            old_value = self.last_values[prop_name]
            new_value = self.widget.get_property(prop_name)
            self.binder.notify_change(bind.ModifyKey(prop_name, new_value))
            self.last_values[prop_name] = new_value
        except:
            with BlockHandler(self.widget, self.handlers[prop_name]):
                self.widget.set_property(prop_name, old_value)
                self.last_values[prop_name] = old_value
            raise
    
    def perform_change(self, change):
        if isinstance(change, bind.SetValue):
            raise Exception("Can't make a PropertyDict the target of a binding "
                    "circuit; this might be changed in the future.")
        elif isinstance(change, bind.DeleteKey):
            raise Exception("Can't delete keys from PropertyDicts")
        elif isinstance(change, bind.ModifyKey):
            if change.key in self.prop_names:
                old_value = self.widget.get_property(change.key)
                with BlockHandler(self.widget, self.handlers[change.key]):
                    self.widget.set_property(change.key, change.value)
                    self.last_values[change.key] = change.value
                def undo():
                    with BlockHandler(self.widget, self.handlers[change.key]):
                        self.widget.set_property(change.key, old_value)
                        self.last_values[change.key] = old_value
                return undo
            else:
                raise Exception("Can't set non-existent property %r" % change.key)
        else:
            raise TypeError
   

class DCheckButton(object):
    def __init__(self):
        self.widget = gtk.CheckButton("")
        self.props = PropertyDict(self.widget)
#        self.label = _PropertyValue(self.widget, "label")
#        self.active = _PropertyValue(self.widget, "active")


class DEntry(object):
    def __init__(self):
        self.widget = gtk.Entry()
        self.props = PropertyDict(self.widget)
#        self.text = _PropertyValue(self.widget, "text")
        #self.placeholder = _PropertyValue(self.widget, "placeholder-text")

    
