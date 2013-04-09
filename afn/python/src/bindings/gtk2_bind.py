
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


class ChildList(bind.PyListMixin, bind.Bindable):
    def __init__(self, dwidget):
        self.dwidget = dwidget
        self.widget = dwidget.widget
        self.children = []
    
    def get_value(self):
        # TODO: Figure out how to not copy and instead return a read-only
        # wrapper, maybe use SimpleSequence to do so
        return self.children[:]
    
    def perform_change(self, change):
        if isinstance(change, bind.InsertItem):
            if change.item.widget.parent:
                raise Exception("Can't add a widget that already has a parent")
            self.children.insert(change.index, change.item)
            self.widget.add(change.item.widget)
            self.widget.reorder_item(change.item.widget, change.index)
            return lambda: self.perform_change(bind.DeleteItem(change.index))
        elif isinstance(change, bind.ReplaceItem):
            with bind.Log() as l:
                l.add(self.perform_change(bind.DeleteItem(change.index)))
                l.add(self.perform_change(bind.InsertItem(change.index, change.item)))
                return l
        elif isinstance(change, bind.DeleteItem):
            item = self.children[change.index]
            del self.children[change.index]
            self.widget.remove(item.widget)
            return lambda: self.perform_change(bind.InsertItem(change.index, item))


class DWidget(object):
    def __init__(self, widget):
        self.widget = widget
        widget.show()
        self.props = PropertyDict(self.widget)
        self.child_props = bind.MemoryDict()
        self.sensitive = bind.value_for_dict_key(self.props, "")


class DContainer(DWidget):
    def __init__(self, widget):
        DWidget.__init__(self, widget)
        self.children = ChildList(self)


class DCheckButton(DWidget):
    def __init__(self):
        DWidget.__init__(self, gtk.CheckButton(""))
        self.label = bind.value_for_dict_key(self.props, "label")
        self.active = bind.value_for_dict_key(self.props, "active")
        self.checked = self.active


class DEntry(DWidget):
    def __init__(self):
        DWidget.__init__(self, gtk.Entry())
        self.text = bind.value_for_dict_key(self.props, "text")
        self.placeholder = bind.value_for_dict_key(self.props, "placeholder-text")


class DWindow(DContainer):
    def __init__(self):
        DContainer.__init__(self, gtk.Window())
        self.title = bind.value_for_dict_key(self.props, "title")


class DBox(DContainer):
    def __init__(self, widget):
        DContainer.__init__(self, widget)
        self.homogeneous = bind.value_for_dict_key(self.props, "homogeneous")


class DHBox(DBox):
    def __init__(self):
        DBox.__init__(self, gtk.HBox())


class DVBox(DBox):
    def __init__(self):
        DBox.__init__(self, gtk.VBox())








