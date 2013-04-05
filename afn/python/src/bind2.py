
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
        if not callable(function):
            raise Exception("Need a callable object, not %r" % function)
        # Functions added this way are performed last to first
        self.functions.append(function)
    
    def then(self, function):
        if not callable(function):
            raise Exception("Need a callable object, not %r" % function)
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
    return a.binder.bind(b.binder)


def unbind(a, b):
    return a.binder.unbind(b.binder)


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
        old = self._value
        self._value = change.value
        def undo():
            self._value = old
        return undo
    
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
        return lambda: None
    
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
    
    def __str__(self):
        try:
            return "<PyDict %s: %r>" % (hex(id(self)), self.binder.get_value())
        except SyntheticError:
            return "<PyDict %s: synthetic>" % hex(id(self))
    
    __repr__ = __str__


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


class SyntheticBindable(Bindable):
    # Bindable that can take on any sort of type, and exists mainly to serve
    # as a placeholder to bind other things to
    def __init__(self):
        self.binder = Binder(self)
    
    def get_value(self):
        raise SyntheticError
    
    def perform_change(self, change):
        return lambda: None


class _ValueUnwrapperValue(Bindable):
    def __init__(self, controller):
        self.binder = Binder(self)
        self.controller = controller
        self._last_value = None
    
    def get_value(self):
        raise SyntheticError
    
    def perform_change(self, change):
        # FIXME: Need to have proper revert support here
        with Log() as l:
            if isinstance(change, SetValue):
                if self._last_value is not None:
                    l.add(unbind(self.controller.binding, self._last_value))
                old_last_value = self._last_value
                self._last_value = change.value
                @l.add
                def _():
                    self._last_value = old_last_value
                l.add(bind(self.controller.binding, change.value))
            elif isinstance(change, LostValue):
                if self._last_value is not None:
                    l.add(unbind(self.controller.binding, self._last_value))
                old_last_value = self._last_value
                self._last_value = None
                @l.add
                def _():
                    self._last_value = old_last_value
            else:
                raise TypeError("Need a SetValue, not %r" % change)
            return l


class ValueUnwrapperController(object):
    def __init__(self):
        self.value = _ValueUnwrapperValue(self)
        self.binding = SyntheticBindable()


class _DictControllerKey(Bindable):
    def __init__(self, controller, key):
        self.binder = Binder(self)
        self.controller = controller
        self._key = key
    
    def get_value(self):
        return self._key
    
    def perform_change(self, change):
        with Log() as l:
            # Update the key. We use l.then instead of l.add as this updates
            # the get_value() value of our _DictControllerValue, which must be
            # changed back before we notify any of its binders of the
            # reversion.
            old_key = self._key
            self._key = change.value
            @l.then
            def _():
                self._key = old_key
            if not self.controller.dict.binder.is_synthetic:
                c = SetValue(self.controller.value.get_value())
                # Notify, not perform; we don't want the value itself trying
                # to perform the change, which would cause it to try to replace
                # the value in the dict, which is pointless.
                l.then(self.controller.value.binder.notify_change(c))
            return l


class _DictControllerValue(Bindable):
    def __init__(self, controller):
        self.binder = Binder(self)
        self.controller = controller
    
    def get_value(self):
        # Will throw SyntheticError if it's synthetic, which we'll propagate
        d = self.controller.dict.binder.get_value()
        if self.controller.key._key in d:
            return d[self.controller.key._key]
        else:
            return self.controller._sentinel
    
    def perform_change(self, change):
        if self.controller.dict.binder.is_synthetic:
            # Dict is synthetic, nothing to change
            return Log()
        if change.value == self.controller._sentinel:
            c = DeleteKey(self.controller.key._key)
        else:
            c = ModifyKey(self.controller.key._key, change.value)
        # Just notify the dict of the change.
        return self.controller.dict.binder.notify_change(c)


class _DictControllerDict(Bindable):
    def __init__(self, controller):
        self.binder = Binder(self)
        self.controller = controller
    
    def get_value(self):
        raise SyntheticError
    
    def perform_change(self, change):
        if isinstance(change, LostValue):
            # Becoming synthetic. Tell the value that we're becoming synthetic,
            # and we're done.
            return self.controller.value.binder.notify_change(LostValue())
        elif isinstance(change, (ModifyKey, DeleteKey)):
            if change.key != self.controller.key._key:
                # Change to a key that we're not looking at; do nothing
                return Log()
            # Change to the key we're looking at
            if isinstance(change, ModifyKey):
                # Key has a value or changed values; notify the value of our
                # change
                return self.controller.value.binder.notify_change(SetValue(change.value))
            else:
                # Key was deleted; set to the sentinel
                return self.controller.value.binder.notify_change(SetValue(self.controller._sentinel))
        elif isinstance(change, SetValue):
            # Figure out what our own key is in the given dict, and notify the
            # value of it. Note that this could result in double notification
            # if we keep getting set to a new dict with our value the same both
            # times, but I don't think double notifications should be a problem
            # for now.
            # TODO: Split this into multiple statements, this line is way too
            # long
            return self.controller.value.binder.notify_change(SetValue(change.value.get(self.controller.key._key, self.controller._sentinel)))
        else:
            raise TypeError


class DictController(object):
    # key is the key to start out with. sentinel is the value to expose (and to
    # watch for) when the key doesn't exist (or to delete the key). I may add
    # support later for not having a sentinel, i.e. setting to a nonexistent
    # key either fails validation or automatically creates it, probably by
    # default the former with an option to use the latter. 
    def __init__(self, key, sentinel=None):
        self.dict = _DictControllerDict(self)
        self.value = _DictControllerValue(self)
        self.key = _DictControllerKey(self, key)
        self._sentinel = sentinel







































    
    