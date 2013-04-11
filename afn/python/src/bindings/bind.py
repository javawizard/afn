
from collections import namedtuple
import collections
import weakref

SetValue = namedtuple("SetValue", ["value"])
# Change that indicates that our circuit is becoming synthetic. SetValue will
# be issued when it becomes concrete again.
LostValue = namedtuple("LostValue", [])

ModifyKey = namedtuple("ModifyKey", ["key", "value"])
DeleteKey = namedtuple("DeleteKey", ["key"])

InsertItem = namedtuple("InsertItem", ["index", "item"])
ReplaceItem = namedtuple("ReplaceItem", ["index", "item"])
DeleteItem = namedtuple("DeleteItem", ["index"])

NoValue = namedtuple("NoValue", [])

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


class IdHash(object):
    __slots__ = ["value"]
    
    def __init__(self, value):
        self.value = value
    
    def __hash__(self):
        return id(self.value)
    
    def __eq__(self, other):
        if not isinstance(other, IdHash):
            return NotImplemented
        return self.__hash__() == other.__hash__()
    
    def __ne__(self, other):
        if not isinstance(other, IdHash):
            return NotImplemented
        return self.__hash__() != other.__hash__()


class StrongWeakSet(collections.MutableSet):
    def __init__(self):
        self._weak = weakref.WeakSet()
        self._strong = set()
    
    def add(self, item, weak=False):
        # This is thread-safe; since we have a strong reference to the item,
        # it won't just suddenly disappear from _weak if it's in there
        if item in self._weak or item in self._strong:
            pass
        if weak:
            self._weak.add(item)
        else:
            self._strong.add(item)
    
    def is_weak(self, item):
        return item in self._weak
    
    def discard(self, item):
        self._weak.discard(item)
        self._strong.discard(item)
    
    def __len__(self):
        return len(self._weak) + len(self._strong)
    
    def __iter__(self):
        for w in self._weak:
            yield w
        for s in self._strong:
            yield s
    
    def __contains__(self, item):
        return item in self._strong or item in self._weak
    
    def __str__(self):
        return "<StrongWeakSet: strong: %r, weak: %r>" % (self._strong, set(self._weak))
    
    __repr__ = __str__


class SimpleMapping(collections.Mapping):
    def __init__(self, get_function, iter_function):
        self.get_function = get_function
        self.iter_function = iter_function
    
    def __getitem__(self, key):
        return self.get_function(key)
    
    def __len__(self):
        i = 0
        for _ in self.iter_function():
            i += 1
        return i
    
    def __iter__(self):
        return self.iter_function()
    
    def __contains__(self, key):
        for k in self.iter_function():
            if k == key:
                return True
        return False


class SimpleSequence(collections.Sequence):
    def __init__(self, get_function, len_function):
        self.get_function
        self.len_function
    
    def __len__(self):
        return self.len_function()
    
    def __getitem__(self, index):
        return self.get_function(index)


class Bindable(object):
    def perform_change(self, change):
        raise NotImplementedError
    
    def get_value(self):
        raise NotImplementedError
    
    @property
    def binder(self):
        try:
            return self._binder
        except AttributeError:
            self._binder = Binder(self)
            return self._binder
    
    @property
    def is_synthetic(self):
        try:
            self.get_value()
            return False
        except SyntheticError:
            return True

# BIG NOTE ABOUT WEAK REFERENCES: Do /not/ weakly bind to a concrete binder
# from a synthetic one. Or in other words, if a binder that's weakly bound to
# is the last concrete binder in its circuit and it goes away, the rest of the
# circuit will /not/ be notified that it's now synthetic (there are far too
# many threading issues involved in doing this), so make sure that there's at
# least one concrete binder on the strong side of the circuit, or that both
# sides are synthetic. I might consider having warnings be printed out if the
# situation is detected, but they still won't be 100% reliable due to threading
# issues.

class Binder(object):
    def __init__(self, bindable):
        self.binders = StrongWeakSet()
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
        
    def bind(self, other, self_weak=False, other_weak=False):
        if other in self.binders: # Already bound, throw an exception
            raise Exception("Already bound")
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
            self.binders.add(other, other_weak)
            other.binders.add(self, self_weak)
            @l1.then
            def _():
                other.binders.discard(self)
                self.binders.discard(other)
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
        if other not in self.binders: # Not bound, throw an exception
            raise Exception("Not bound")
        with Log() as l1:
            self_weak, other_weak = other.binders.is_weak(self), self.binders.is_weak(other)
            self.binders.discard(other)
            other.binders.discard(self)
            @l1.then
            def _():
                other.binders.add(self, self_weak)
                self.binders.add(other, other_weak)
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


def bind(a, b, a_weak=False, b_weak=False):
    return b.binder.bind(a.binder, b_weak, a_weak)

s_bind_s = bind

def s_bind_w(a, b):
    return bind(a, b, False, True)

def w_bind_s(a, b):
    return bind(a, b, True, False)

def w_bind_w(a, b):
    return bind(a, b, True, True)


def unbind(a, b):
    return a.binder.unbind(b.binder)


class SyntheticBindable(Bindable):
    # Bindable that can take on any sort of type, and exists mainly to serve
    # as a placeholder to bind other things to
    def get_value(self):
        raise SyntheticError
    
    def perform_change(self, change):
        return Log()


def value_str(value_bindable):
    try:
        return "<%s: %r>" % (type(value_bindable).__name__, value_bindable.binder.get_value())
    except SyntheticError:
        return "<%s: synthetic>" % type(value_bindable).__name__


def list_insert(target_list, index, item):
    target_list.insert(index, item)
    def undo():
        del target_list[index]
    return undo


def list_delete(target_list, index):
    item = target_list[index]
    del target_list[index]
    def undo():
        target_list.insert(index, item)
    return undo


def list_replace(target_list, index, item):
    old_item = target_list[index]
    target_list[index] = item
    def undo():
        target_list[index] = old_item
    return undo


def list_str(list_bindable):
    try:
        return "<%s: %r>" % (type(list_bindable).__name__, list(list_bindable.binder.get_value()))
    except SyntheticError:
        return "<%s: synthetic>" % type(list_bindable).__name__


def dict_modify(target_dict, key, value):
    if key in target_dict:
        old_value = target_dict[key]
        target_dict[key] = value
        def undo():
            target_dict[key] = old_value
        return undo
    else:
        target_dict[key] = value
        def undo():
            del target_dict[key]


def dict_delete(target_dict, key):
    if key not in target_dict:
        raise KeyError(key)
    old_value = target_dict[key]
    del target_dict[key]
    def undo():
        target_dict[key] = old_value
    return undo


def dict_str(dict_bindable):
    try:
        return "<%s: %r>" % (type(dict_bindable).__name__, dict(dict_bindable.binder.get_value()))
    except SyntheticError:
        return "<%s: synthetic>" % type(dict_bindable).__name__


class PyValueMixin(Bindable):
    # Mixin that exposes a Python-friendly interface for value bindables, in
    # the form of get and set functions and a value property
    def get(self):
        return self.binder.get_value()
    
    def set(self, value):
        self.binder.perform_change(SetValue(value))
    
    value = property(get, set)
    
    __str__ = value_str
    __repr__ = value_str


class MemoryValue(PyValueMixin, Bindable):
    # Concrete value that stores its value in memory
    def __init__(self, value):
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


class PyValue(SyntheticBindable, PyValueMixin):
    # Synthetic value that allows the convenient interface of PyValueMixin to
    # be used with anything that can be bound to it
    def __init__(self, bindable):
        if bindable is not None:
            w_bind_s(self, bindable)


class PyDictMixin(Bindable, collections.MutableMapping):
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
    
    __str__ = dict_str
    __repr__ = dict_str


class MemoryDict(PyDictMixin, Bindable):
    # Dictionary bindable that stores things in memory
    def __init__(self):
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


class PyDict(SyntheticBindable, PyDictMixin):
    # Synthetic bindable that exposes a collections.MutableMapping-compatible
    # interface for any other dictionary bindable
    def __init__(self, bindable=None):
        if bindable is not None:
            w_bind_s(self, bindable)


class EmptyDict(Bindable):
    def get_value(self):
        return {}
    
    def perform_change(self, change):
        if isinstance(change, SetValue) and len(change.value) == 0:
            return Log()
        else:
            raise Exception("instances of EmptyDict can't be modified")


class PyListMixin(Bindable, collections.MutableSequence):
    def __getitem__(self, index):
        return self.binder.get_value()[index]
    
    def __len__(self):
        return len(self.binder.get_value())
    
    def __setitem__(self, index, item):
        self.binder.perform_change(ReplaceItem(index, item))
    
    def __delitem__(self, index):
        self.binder.perform_change(DeleteItem(index))
    
    def insert(self, index, item):
        self.binder.perform_change(InsertItem(index, item))
    
    __str__ = list_str
    __repr__ = list_str


class MemoryList(PyListMixin, Bindable):
    def __init__(self):
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


class ListenerBindable(Bindable):
    def __init__(self, listener):
        self.listener = listener
    
    def get_value(self):
        raise SyntheticError
    
    def perform_change(self, change):
        if self.listener is not None:
            self.listener(change)


class _ValueUnwrapperValue(Bindable):
    def __init__(self, controller):
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
                l.add(bind(change.value, self.controller.binding))
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


class _DictControllerKey(PyValueMixin, Bindable):
    def __init__(self, controller, key):
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


class _DictControllerValue(PyValueMixin, Bindable):
    def __init__(self, controller):
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
    def __init__(self, key, sentinel=NoValue()):
        self.dict = _DictControllerDict(self)
        self.value = _DictControllerValue(self)
        self.key = _DictControllerKey(self, key)
        self._sentinel = sentinel


def value_for_dict_key(d, key, sentinel=NoValue()):
    controller = DictController(key, sentinel)
    s_bind_s(controller.dict, d)
    return controller.value


def value_for_weak_dict_key(d, key, sentinel=NoValue()):
    controller = DictController(key, sentinel)
    w_bind_s(controller.dict, d)
    return controller.value


class _ListTranslatorList(MemoryList):
    def __init__(self, from_function):
        MemoryList.__init__(self)
        self.from_function = from_function
        self.items = []
    
    def perform_change(self, change):
        with Log() as l:
            if isinstance(change, InsertItem):
                l.then(MemoryList.perform_change(self, change))
                translated_change = InsertItem(change.index, self.from_function(change.item))
                l.then(MemoryList.perform_change(self.other, translated_change))
                l.then(self.other.binder.notify_change(translated_change))
            elif isinstance(change, ReplaceItem):
                l.then(MemoryList.perform_change(self, change))
                translated_change = ReplaceItem(change.index, self.from_function(change.item))
                l.then(MemoryList.perform_change(self.other, translated_change))
                l.then(self.other.binder.notify_change(translated_change))
            elif isinstance(change, DeleteItem):
                l.then(MemoryList.perform_change(self, change))
                l.then(MemoryList.perform_change(self.other, change))
                l.then(self.other.binder.notify_change(change))
            elif isinstance(change, SetValue):
                # TODO: Really ought to split this out for other widgets too lazy
                # to do any special processing to make use of
                for n in reversed(range(len(self.get_value()))):
                    l.add(self.perform_change(DeleteItem(n)))
                for i, v in enumerate(change.value):
                    l.add(self.perform_change(InsertItem(i, v)))
            else:
                raise TypeError
            return l


class ListTranslator(object):
    def __init__(self, a_to_b, b_to_a):
        self.a = _ListTranslatorList(a_to_b)
        self.b = _ListTranslatorList(b_to_a)
        self.a.other = self.b
        self.b.other = self.a














































    
    
