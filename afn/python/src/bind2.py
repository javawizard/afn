
from collections import namedtuple
import collections

SetValue = namedtuple("SetValue", ["value"])
# Change that indicates that our circuit is becoming synthetic. SetValue will
# be issued when it becomes concrete again.
LostValue = namedtuple("LostValue", [])

ModifyKey = namedtuple("ModifyKey", ["key", "value"])
DeleteKey = namedtuple("DeleteKey", ["key"])

InsertItem = namedtuple("InsertItem", ["index", "item"])
ReplaceItem = namedtuple("ReplaceItem", ["index", "item"])
DeleteItem = namedtuple("DeleteItem", ["index"])

class SyntheticError(Exception):
    pass


class Log(object):
    def __init__(self):
        self.functions = []
    
    def __enter__(self):
        return self
    
    def add(self, function):
        # Functions added this way are performed last to first
        self.functions.append(function)
    
    def then(self, function):
        # Functions added this way are performed first to last
        self.functions.insert(0, function)
    
    def __exit__(self, exc_type, *args):
        if exc_type is not None:
            self()
    
    def __call__(self):
        for f in reversed(self.functions):
            f()


class Bindable(object):
    def perform_change(self, change):
        raise NotImplementedError
    
    def get_value(self):
        raise NotImplementedError
    
    @property
    def is_synthetic(self):
        try:
            self.get_value()
            return False
        except SyntheticError:
            return True


class Binder(object):
    def __init__(self, bindable):
        self.binders = []
        self.bindable = bindable
    
    def get_binders(self, binders=None):
        if binders is None:
            binders = set()
        if self not in binders:
            binders.add(self)
            for b in self.binders:
                b.get_binders(binders)
        return binders
    
    def get_value(self):
        for binder in self.get_binders():
            try:
                return binder.bindable.get_value()
            except SyntheticError:
                pass
        raise SyntheticError
    
    @property
    def is_synthetic(self):
        try:
            self.get_value()
            return False
        except SyntheticError:
            return True
        
    def bind(self, other):
        if other in self.binders: # Already bound, don't do anything
            return Log()
        with Log() as l1:
            # Figure out whose value to keep; it's basically other's if we're
            # synthetic but other isn't, and self's otherwise
            if self.is_synthetic and not other.is_synthetic:
                keep, update = other, self
            else:
                keep, update = self, other
            update_binders = update.get_binders()
            if not keep.is_synthetic:
                # We get the value before linking to make sure we get our own
                # value and not the value of the binder we linked to
                keep_value = keep.get_value()
            # Link and add a reversion step that unlinks
            self.binders.append(other)
            other.binders.append(self)
            @l1.then
            def _():
                other.binders.remove(self)
                self.binders.remove(other)
            # If keep's synthetic, then other must be as well, so we're done.
            # If not, though, then we need to pass the new value to all of
            # update's former binders.
            if not keep.is_synthetic:
                # Note that, no matter where we get an exception, concrete
                # binders /must/ be reverted before synthetic ones (so that the
                # synthetic ones see an already-reverted value from their
                # binder's get_value()), so we leave it up to l1's with
                # statement to revert anything we do here.
                l2, l3 = Log(), Log()
                l1.then(l2)
                l1.then(l3)
                for binder in update_binders:
                    if not binder.bindable.is_synthetic:
                        l2.add(binder.bindable.perform_change(SetValue(keep_value)))
                for binder in update_binders:
                    if binder.bindable.is_synthetic:
                        l3.add(binder.bindable.perform_change(SetValue(keep_value)))
            # That should be it. Then we just return l1.
            return l1
    
    def unbind(self, other):
        if other not in self.binders: # Not bound, don't do anything
            return Log()
        with Log() as l1:
            self.binders.remove(other)
            other.binders.remove(self)
            @l1.then
            def _():
                other.binders.append(self)
                self.binders.append(other)
            if (self.is_synthetic and not other.is_synthetic) or (other.is_synthetic and not self.is_synthetic):
                # If they're both synthetic then we don't need to do anything
                # else as we were already synthetic before we unbound. If
                # they're both concrete then both of them will keep their
                # values so we don't need to do anything. But if one is newly
                # synthetic, then we need to let it know that it's now
                # synthetic.
                synthetic = self if self.is_synthetic else other
                l2 = Log()
                l1.then(l2)
                for binder in synthetic.get_binders():
                    l2.add(binder.bindable.perform_change(LostValue()))
            return l1

    def notify_change(self, change, to_self=False):
        if isinstance(change, LostValue) and not self.is_synthetic:
            # We're now synthetic but another binder on our circuit is still
            # concrete, so don't do anything
            return Log()
        with Log() as l1:
            l2, l3 = Log(), Log()
            l1.then(l2)
            l1.then(l3)
            for binder in self.get_binders():
                if (to_self or binder != self) and not binder.bindable.is_synthetic:
                    l2.add(binder.bindable.perform_change(change))
            for binder in self.get_binders():
                if (to_self or binder != self) and binder.bindable.is_synthetic:
                    l3.add(binder.bindable.perform_change(change))
            return l1
    
    def perform_change(self, change):
        return self.notify_change(change, to_self=True)


def bind(a, b):
    a.binder.bind(b.binder)


def unbind(a, b):
    a.binder.unbind(b.binder)


class Value(Bindable):
    def __init__(self, value):
        self.binder = Binder(self)
        self._value = value
    
    def get_value(self):
        return self._value
    
    def perform_change(self, change):
        # We're concrete, so we'll never see a LostValue change
        if not isinstance(change, SetValue):
            raise TypeError("Need a SetValue instance")
        self._value = change.value
    
    @property
    def value(self):
        return self._value
    
    @value.setter
    def value(self, new_value):
        self.binder.perform_change(SetValue(new_value))


class MemoryDict(Bindable):
    # Dictionary bindable that stores things in memory
    def __init__(self):
        self.binder = Binder(self)
        self._dict = {}
    
    def get_value(self):
        return self._dict
    
    def perform_change(self, change):
        print "Change %r to %r" % (change, self)
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
        elif isinstance(change, DeleteKey):
            if change.key in self._dict:
                old = self._dict[change.key]
                del self._dict[change.key]
                def undo():
                    self._dict[change.key] = old
            else: # Delete a non-existent key; throw an exception. Not sure at
                # the moment if we should just ignore it instead... Main
                # advantage to throwing a proper exception is that then
                # PyDict.__del__ correctly throws an exception when trying to
                # delete a key that doesn't actually exist.
                raise KeyError(change.key)
        elif isinstance(change, SetValue):
            old = self._dict.copy()
            self._dict.clear()
            self._dict.update(change.value)
            def undo():
                self._dict.clear()
                self._dict.update(old)
        else:
            raise TypeError("Need a ModifyKey or DeleteKey")
        return undo
    
    def __str__(self):
        return "<MemoryDict %s: %r>" % (hex(id(self)), self._dict)
    
    __repr__ = __str__


class PyDict(Bindable, collections.MutableMapping):
    # Synthetic bindable that exposes a collections.MutableMapping-compatible
    # interface for any other dictionary bindable
    def __init__(self):
        self.binder = Binder(self)
    
    def get_value(self):
        raise SyntheticError
    
    def perform_change(self, change):
        pass
    
    def __getitem__(self, key):
        return self.binder.get_value()[key]
    
    def __setitem__(self, key, value):
        self.binder.perform_change(ModifyKey(key, value))
    
    def __delitem__(self, key):
        self.binder.perform_change(DeleteKey(key))
    
    def __len__(self):
        return len(self.binder.get_value())
    
    def __iter__(self):
        return self.binder.get_value().__iter__()
    
    def __contains__(self, key):
        return self.binder.get_value().__contains__(key)


class MemoryList(Bindable):
    def __init__(self):
        self.binder = Binder(self)
        self._list = []
    
    def get_value(self):
        return self._list
    
    def perform_change(self, change):
        if isinstance(change, InsertItem):
            if change.index < 0 or change.index > len(self._list):
                raise IndexError("Insert index out of range")
            self._list.insert(change.index, change.item)
            def undo():
                del self._list[change.index]
        elif isinstance(change, ReplaceItem):
            if change.index < 0 or change.index >= len(self._list):
                raise IndexError("Replace index out of range")
            old = self._list[change.index]
            self._list[change.index] = change.item
            def undo():
                self._list[change.index] = old
        elif isinstance(change, DeleteItem):
            if change.index < 0 or change.index >= len(self._list):
                raise IndexError("Delete index out of range")
            old = self._list[change.index]
            del self._list[change.index]
            def undo():
                self._list.insert(change.index, old)
        elif isinstance(change, SetValue):
            old = self._list[:]
            self._list[:] = []
            self._list.extend(change.value)
            def undo():
                self._list[:] = []
                self._list.extend(old)
        else:
            raise TypeError("Need a list-related change")
        return undo







































    
    