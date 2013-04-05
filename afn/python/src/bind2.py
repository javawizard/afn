
from collections import namedtuple

SetValue = namedtuple("SetValue", ["value"])

ModifyKey = namedtuple("ModifyKey", ["key", "value"])
DeleteKey = namedtuple("DeleteKey", ["key"])

class Log(object):
    def __enter__(self):
        return self
    
    def add(self, function):
        self.functions.append(function)
    
    def __exit__(self, exc_type, *args):
        if exc_type:
            self()
    
    def __call__(self):
        for f in reversed(self.functions):
            f()


class Bindable(object):
    def perform_change(self, change):
        raise NotImplementedError
    
    def get_value(self):
        raise NotImplementedError


class Binder(object):
    def __init__(self, bindable):
        self.binders = []
        self.bindable = bindable
    
    def get_binders(self):
        binders = set()
        self.do_get_binders(binders)
        return binders
    
    def do_get_binders(self, binders):
        if self in binders:
            return
        binders.add(self)
        for b in self.binders:
            b.do_get_binders(binders)
    
    def perform_change(self, change):
        with Log() as l:
            for binder in self.get_binders():
                l.add(binder.do_perform_change(change))
            return l
    
    def do_perform_change(self, change):
        return self.bindable.perform_change(change)
    
    def get_value(self):
        return self.bindable.get_value()
    
    def bind(self, other):
        if other in self.binders: # Already bound, don't do anything
            return Log()
        with Log() as l:
            # Update other's value to our value
            l.add(other.perform_change(SetValue(self.get_value())))
            # Add ourselves to each other's binder lists
            self.binders.append(other)
            other.binders.append(self)
            @l.add
            def _():
                other.binders.remove(self)
                self.binders.remove(other)
            return l


class Value(Bindable):
    def __init__(self, value):
        self.binder = Binder(self)
        self._value = value
    
    def get_value(self):
        return self._value
    
    def perform_change(self, change):
        if not isinstance(change, SetValue):
            raise TypeError("Need a SetValue instance")
        self._value = change.value
    
    @property
    def value(self):
        return self._value
    
    @value.setter
    def value(self, new_value):
        self.binder.perform_change(SetValue(new_value))


class Dict(Bindable):
    def __init__(self):
        self.binder = Binder(self)
        self._dict = {}
    
    def get_value(self):
        return self._dict
    
    def perform_change(self, change):
        if not isinstance(change, (ModifyKey, DeleteKey)):
            raise TypeError("Need a ModifyKey or DeleteKey")
        if isinstance(change, ModifyKey):
            if change.key in self._dict:
                old = self._dict[change.key]
                self._dict[change.key] = change.value
                def undo():
                    self._dict[change.key] = old
            else:
                self._dict[change.key] = change.value
                def undo():
                    del self._dict[change.key]
        else:
            if change.key in self._dict:
                old = self._dict[change.key]
                del self._dict[change.key]
                def undo():
                    self._dict[change.key] = old
            else: # Delete a non-existent key; no-op
                undo = lambda: None
        return undo






































    
    