
from bindings import bind
from functools import partial
import gobject
import gtk

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


class ChildPropertyDict(bind.PyDictMixin, bind.Bindable):
    def __init__(self, widget, child):
        self.widget = widget
        self.child = child
        self.prop_names = [p.name for p in widget.list_child_properties() if p.flags & gobject.PARAM_READABLE]
        self.handlers = {}
        self.last_values = dict((p, self.widget.child_get_property(child, p)) for p in self.prop_names)
        self.connected = False
        self.connect()
    
    def connect(self):
        if self.connected:
            raise Exception("Already connected")
        self.connected = True
        for prop_name in self.prop_names:
            self.handlers[prop_name] = self.widget.connect("child-notify::" + prop_name, partial(self.signal, prop_name))
    
    def disconnect(self):
        if not self.connected:
            raise Exception("Not connected")
        self.connected = False
        for handler in self.handlers.values():
            self.widget.disconnect(handler)
    
    def get_value(self):
        return bind.SimpleMapping(self._get_function, lambda: iter(self.prop_names))
    
    def _get_function(self, key):
        if key not in self.prop_names:
            raise KeyError(key)
        return self.widget.child_get_property(self.child, key)
    
    def signal(self, prop_name, *args):
        try:
            old_value = self.last_values[prop_name]
            new_value = self.widget.child_get_property(self.child, prop_name)
            self.binder.notify_change(bind.ModifyKey(prop_name, new_value))
            self.last_values[prop_name] = new_value
        except:
            with BlockHandler(self.widget, self.handlers[prop_name]):
                self.widget.child_set_property(self.child, prop_name, old_value)
                self.last_values[prop_name] = old_value
            raise
    
    def perform_change(self, change):
        if isinstance(change, bind.SetValue):
            raise Exception("Can't make a ChildPropertyDict the target of a "
                            "binding circuit; this might be chnaged in the future.")
        elif isinstance(change, bind.DeleteKey):
            raise Exception("Can't delete keys from ChildPropertyDicts")
        elif isinstance(change, bind.ModifyKey):
            if change.key in self.prop_names:
                old_value = self.widget.child_get_property(self.child, change.key)
                with BlockHandler(self.widget, self.handlers[change.key]):
                    self.widget.child_set_property(self.child, change.key, change.value)
                    self.last_values[change.key] = change.value
                def undo():
                    with BlockHandler(self.widget, self.handlers[change.key]):
                        self.widget.child_set_property(self.child, change.key, old_value)
                        self.last_values[change.key] = old_value
                return undo
            else:
                raise Exception("Can't set non-existent child property %r" % change.key)
        else:
            raise TypeError


class ChildList(bind.PyListMixin, bind.Bindable):
    def __init__(self, dwidget):
        self.dwidget = dwidget
        self.widget = dwidget.widget
        self.children = []
        self.child_prop_dicts = []
    
    def get_value(self):
        # TODO: Figure out how to not copy and instead return a read-only
        # wrapper, maybe use SimpleSequence to do so
        return self.children[:]
    
    def perform_change(self, change):
        if isinstance(change, bind.InsertItem):
            if change.item.widget.parent:
                raise Exception("Can't add a widget that already has a parent")
            if isinstance(self.widget, gtk.Bin) and len(self.children) == 1:
                raise Exception("Can't add more than one child to a bin")
            self.children.insert(change.index, change.item)
            self.widget.add(change.item.widget)
            if change.index < (len(self.children) - 1):
                self.widget.reorder_child(change.item.widget, change.index)
            # Create a ChildPropertyDict for this child
            self.child_prop_dicts.insert(change.index, ChildPropertyDict(self.widget, change.item.widget))
            # Copy all of the child's current child properties that we know about
            for k in self.child_prop_dicts[change.index].keys():
                if k in change.item.child_props:
                    self.child_prop_dicts[change.index][k] = change.item.child_props[k]
            # Then bind the child dict to the child's child dict
            bind.bind(change.item.child_props, self.child_prop_dicts[change.index]) 
            return lambda: self.perform_change(bind.DeleteItem(change.index))
        elif isinstance(change, bind.ReplaceItem):
            with bind.Log() as l:
                l.add(self.perform_change(bind.DeleteItem(change.index)))
                l.add(self.perform_change(bind.InsertItem(change.index, change.item)))
                return l
        elif isinstance(change, bind.DeleteItem):
            item = self.children[change.index]
            del self.children[change.index]
            # Unbind our ChildPropertyDict from the widget's child dict
            bind.unbind(item.child_props, self.child_prop_dicts[change.index])
            # Disconnect our ChildPropertyDict and then delete it
            self.child_prop_dicts[change.index].disconnect()
            del self.child_prop_dicts[change.index]
            # Then remove the actual widget
            self.widget.remove(item.widget)
            return lambda: self.perform_change(bind.InsertItem(change.index, item))
        elif isinstance(change, bind.SetValue):
            # TODO: Really ought to split this out for other widgets too lazy
            # to do any special processing to make use of
            with bind.Log() as l:
                for n in reversed(range(len(self.children))):
                    l.add(self.perform_change(bind.DeleteItem(n)))
                for i, v in enumerate(change.value):
                    l.add(self.perform_change(bind.InsertItem(i, v)))
                return l
        else:
            raise TypeError
    
    __str__ = bind.list_str
    __repr__ = bind.list_str


class DWidget(bind.AttributeDict):
    def __init__(self, widget):
        bind.AttributeDict.__init__(self)
        self.widget = widget
        if not hasattr(type(self), "_do_not_show"):
            widget.show()
        self.props = PropertyDict(self.widget)
        self.child_props = bind.PyDict()
        bind.key_bind(self, "sensitive", self.props, "sensitive")
        bind.key_bind(self, "visible", self.props, "visible")


class DContainer(DWidget):
    def __init__(self, widget):
        DWidget.__init__(self, widget)
        self.children = ChildList(self)


class DCheckButton(DWidget):
    def __init__(self):
        DWidget.__init__(self, gtk.CheckButton(""))
        bind.key_bind(self, "label", self.props, "label")
        bind.key_bind(self, "active", self.props, "active")
        bind.key_bind(self, "checked", self, "active")


class DButton(DWidget):
    def __init__(self):
        DWidget.__init__(self, gtk.Button(""))
        bind.key_bind(self, "label", self.props, "label")


class DEntry(DWidget):
    def __init__(self):
        DWidget.__init__(self, gtk.Entry())
        bind.key_bind(self, "text", self.props, "text")
        bind.key_bind(self, "placeholder", self.props, "placeholder")
        self._delayed = bind.DelayController()
        bind.bind(self._delayed.model, bind.MemoryValue(""))
        bind.bind(self._delayed.view, bind.value_for_dict_key(self, "text"))
        bind.bind(self._delayed.model, bind.value_for_dict_key(self, "delayed_text"))
        self._delayed.save()
        self.widget.connect("focus-out-event", self._save_and_false)
        self.widget.connect("activate", self._save_and_false)
        self.widget.connect("key-press-event", self._key_press)
    
    def _save_and_false(self, *args):
        self._delayed.save()
        return False
    
    def _key_press(self, widget, event):
        if event.keyval == gtk.keysyms.Escape:
            self._delayed.cancel()
    
    


class DLabel(DWidget):
    def __init__(self):
        DWidget.__init__(self, gtk.Label())
        bind.key_bind(self, "label", self.props, "label")
        bind.key_bind(self, "text", self, "label")


class DWindow(DContainer):
    def __init__(self):
        DContainer.__init__(self, gtk.Window())
        bind.key_bind(self, "title", self.props, "title")


class DBox(DContainer):
    def __init__(self, widget):
        DContainer.__init__(self, widget)
        bind.key_bind(self, "homogeneous", self.props, "homogeneous")


class DHBox(DBox):
    def __init__(self):
        DBox.__init__(self, gtk.HBox())


class DVBox(DBox):
    def __init__(self):
        DBox.__init__(self, gtk.VBox())


class DScrolledWindow(DContainer):
    def __init__(self):
        DContainer.__init__(self, gtk.ScrolledWindow())


class DViewport(DContainer):
    def __init__(self):
        DContainer.__init__(self, gtk.Viewport())


class DMenu(DContainer):
    _do_not_show = True
    def __init__(self):
        DContainer.__init__(self, gtk.Menu())


class DMenuItem(DWidget):
    def __init__(self, widget=None):
        DWidget.__init__(self, widget or gtk.MenuItem())
        bind.key_bind(self, "label", self.props, "label")


class DCheckMenuItem(DMenuItem):
    def __init__(self, widget=None):
        DMenuItem.__init__(self, widget or gtk.CheckMenuItem())
        bind.key_bind(self, "active", self.props, "active")
        bind.key_bind(self, "checked", self, "active")


class DMenuBar(DContainer):
    def __init__(self):
        DContainer.__init__(self, gtk.MenuBar())


def make(widget, props={}, child_props={}, children=[]):
    widget.props.update(props)
    widget.child_props.update(child_props)
    if children:
        widget.children.extend(children)
    return widget








