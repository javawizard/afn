
from collections import namedtuple
import collections
import weakref
from stm_system.datatypes.tlist import TList
from stm_system.datatypes.tdict import TDict
from stm_system.datatypes.tset import TSet
from stm_system.datatypes.tobject import TObject
from stm_system import stm

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


class IdHash(object):
    __slots__ = ["value"]
    
    def __init__(self, value):
        self.value = value
    
    def __hash__(self):
        return id(self.value)
    
    def __cmp__(self, other):
        if not isinstance(other, IdHash):
            return NotImplemented
        return cmp(self.__hash__(), other.__hash__())


class StrongWeakSet(collections.MutableSet):
    def __init__(self):
        self._weak = TSet(backing_type=weakref.WeakSet)
        self._strong = TSet(backing_type=set)
    
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
        return "<StrongWeakSet: strong: %r, weak: %r>" % (self._strong, self._weak)
    
    __repr__ = __str__


# Probably not needed anymore, since copies of TDicts are O(1). TODO: Actually
# implement TDict.copy() to just create a copy with the same current root node.
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


# Same note as SimpleMapping about not being needed
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
        # TODO: Make sure we don't need to subclass TObject for this to work
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
            binders = TSet()
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
        @stm.atomically
        def _():
            if other in self.binders: # Already bound, throw an exception
                raise Exception("Already bound")
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
            # If keep's synthetic, then other must be as well, so we're done.
            # If not, though, then we need to pass the new value to all of
            # update's former binders.
            if not keep.is_synthetic:
                for binder in update_binders:
                    if not binder.bindable.is_synthetic:
                        binder.bindable.perform_change(SetValue(keep_value))
                for binder in update_binders:
                    if binder.bindable.is_synthetic:
                        binder.bindable.perform_change(SetValue(keep_value))
            # That should be it.
    
    def unbind(self, other):
        @stm.atomically
        def _():
            if other not in self.binders: # Not bound, throw an exception
                raise Exception("Not bound")
            self_weak, other_weak = other.binders.is_weak(self), self.binders.is_weak(other)
            self.binders.discard(other)
            other.binders.discard(self)
            if (self.is_synthetic and not other.is_synthetic) or (other.is_synthetic and not self.is_synthetic):
                # If they're both synthetic then we don't need to do anything
                # else as we were already synthetic before we unbound. If
                # they're both concrete then both of them will keep their
                # values so we don't need to do anything. But if one is newly
                # synthetic, then we need to let it know that it's now
                # synthetic.
                synthetic = self if self.is_synthetic else other
                for binder in synthetic.get_binders():
                    binder.bindable.perform_change(LostValue())

    def notify_change(self, change, to_self=False):
        @stm.atomically
        def _():
            if isinstance(change, LostValue) and not self.is_synthetic:
                # We're now synthetic but another binder on our circuit is still
                # concrete, so don't do anything
                return
            for binder in self.get_binders():
                if (to_self or binder != self) and not binder.bindable.is_synthetic:
                    binder.bindable.perform_change(change)
            for binder in self.get_binders():
                if (to_self or binder != self) and binder.bindable.is_synthetic:
                    binder.bindable.perform_change(change)
    
    def perform_change(self, change):
        self.notify_change(change, to_self=True)


def bind(s, m, s_weak=True, m_weak=False):
    m.binder.bind(s.binder, m_weak, s_weak)

def s_bind_s(s, m):
    bind(s, m, False, False)

def s_bind_w(s, m):
    bind(s, m, False, True)

def w_bind_s(s, m):
    bind(s, m, True, False)

def w_bind_w(s, m):
    bind(s, m, True, True)


def unbind(a, b):
    a.binder.unbind(b.binder)


class SyntheticBindable(Bindable):
    # Bindable that can take on any sort of type, and exists mainly to serve
    # as a placeholder to bind other things to
    def get_value(self):
        raise SyntheticError
    
    def perform_change(self, change):
        pass


def value_str(value_bindable):
    @stm.atomically
    def _():
        try:
            return "<%s: %r>" % (type(value_bindable).__name__, value_bindable.binder.get_value())
        except SyntheticError:
            return "<%s: synthetic>" % type(value_bindable).__name__


def list_str(list_bindable):
    @stm.atomically
    def _():
        try:
            return "<%s: %r>" % (type(list_bindable).__name__, list(list_bindable.binder.get_value()))
        except SyntheticError:
            return "<%s: synthetic>" % type(list_bindable).__name__


def dict_str(dict_bindable):
    @stm.atomically
    def _():
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
        self._var = stm.TVar(value)
    
    def get_value(self):
        return self._var.get()
    
    def perform_change(self, change):
        # We're concrete, so we'll never see a LostValue change
        if not isinstance(change, SetValue):
            raise TypeError("Need a SetValue instance")
        self._var.set(change.value)


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


class MemoryDict(Bindable):
    # Dictionary bindable that stores things in memory
    def __init__(self):
        self._dict = TDict()
    
    def get_value(self):
        # TODO: return self._dict.copy(), which will be O(1) once I properly
        # implement it
        return self._dict
    
    def perform_change(self, change):
        if isinstance(change, ModifyKey):
            self._dict[change.key] = change.value
        elif isinstance(change, DeleteKey):
            if change.key in self._dict:
                del self._dict[change.key]
            else: # Delete a non-existent key; throw an exception. Not sure at
                # the moment if we should just ignore it instead... Main
                # advantage to throwing a proper exception is that then
                # PyDict.__del__ correctly throws an exception when trying to
                # delete a key that doesn't actually exist.
                # UPDATE: Ignoring for now, as this causes problems with
                # DictController (setting the value to the delete-key sentinel
                # when it's already set to the sentinel causes a duplicate
                # DeleteKey to be issued to whatever's bound to the
                # controller's dict). But I've yet to see anything actually
                # relying on __del__ throwing an exception, so this should be
                # fine. And I'll revisit this if it becomes a problem later.
                pass
        elif isinstance(change, SetValue):
            # TODO: Might want to have TDict.update check to see if the other
            # dictionary is also a TDict and, if so, just copy its node, and if
            # not, invoke the superclass update implementation
            self._dict.clear()
            self._dict.update(change.value)
        else:
            raise TypeError("Need a ModifyKey or DeleteKey")


class PyDict(MemoryDict, PyDictMixin):
    # Synthetic bindable that exposes a collections.MutableMapping-compatible
    # interface for any other dictionary bindable
    def __init__(self, bindable=None):
        MemoryDict.__init__(self)
        if bindable is not None:
            w_bind_s(self, bindable)


class AttributeDict(MemoryDict):
    def __getattr__(self, key):
        try:
            return self.get_value()[key]
        except KeyError as e:
            raise AttributeError(e.args[0])
    
    def __setattr__(self, key, value):
        if key == "_dict" or key == "_binder":
            # Used by superclasses
            object.__setattr__(self, key, value)
        else:
            self.binder.perform_change(ModifyKey(key, value))
    
    def __delattr__(self, key):
        self.binder.perform_change(DeleteKey(key))
    
    def __dir__(self):
        return self.get_value().items()


class EmptyDict(Bindable):
    def get_value(self):
        return {}
    
    def perform_change(self, change):
        if isinstance(change, SetValue) and len(change.value) == 0:
            return
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


class MemoryList(Bindable):
    def __init__(self):
        self._list = TList()
    
    def get_value(self):
        # TODO: Add a copy() to TList and use this, which'll be O(1)
        return self._list
    
    def perform_change(self, change):
        if isinstance(change, InsertItem):
            self._list.insert(change.index, change.item)
        elif isinstance(change, ReplaceItem):
            self._list[change.index] = change.item
        elif isinstance(change, DeleteItem):
            del self._list[change.index]
        elif isinstance(change, SetValue):
            self._list[:] = []
            self._list.extend(change.value)
        else:
            raise TypeError("Need a list-related change")


class PyList(MemoryList, PyListMixin):
    pass


class ListenerBindable(Bindable):
    def __init__(self, listener):
        self.listener = listener
    
    def get_value(self):
        raise SyntheticError
    
    def perform_change(self, change):
        if self.listener is not None:
            self.listener(change)


class _ValueUnwrapperValue(Bindable, TObject):
    def __init__(self, controller):
        TObject.__init__(self)
        self.controller = controller
        self._last_value = None
    
    def get_value(self):
        raise SyntheticError
    
    def perform_change(self, change):
        @stm.atomically
        def _():
            if isinstance(change, SetValue):
                if self._last_value is not None:
                    unbind(self.controller.binding, self._last_value)
                self._last_value = change.value
                # FIXME: Need to figure out what type of binding we're supposed
                # to be doing here (strong/weak), maybe allow the
                # ValueUnwrapperController constructor to specify, or just do
                # as-is for now, I dunno...
                bind(change.value, self.controller.binding)
            elif isinstance(change, LostValue):
                if self._last_value is not None:
                    unbind(self.controller.binding, self._last_value)
                self._last_value = None
            else:
                raise TypeError("Need a SetValue, not %r" % change)


class ValueUnwrapperController(object):
    def __init__(self):
        self.value = _ValueUnwrapperValue(self)
        self.binding = SyntheticBindable()


class _DictControllerKey(PyValueMixin, Bindable, TObject):
    def __init__(self, controller, key):
        TObject.__init__(self)
        self.controller = controller
        self._key = key
    
    def get_value(self):
        return self._key
    
    def perform_change(self, change):
        @stm.atomically
        def _():
            self._key = change.value
            if not self.controller.dict.binder.is_synthetic:
                c = SetValue(self.controller.value.get_value())
                # Notify, not perform; we don't want the value itself trying
                # to perform the change, which would cause it to try to replace
                # the value in the dict, which is pointless.
                self.controller.value.binder.notify_change(c)


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
            return
        if change.value == self.controller._sentinel:
            c = DeleteKey(self.controller.key._key)
        else:
            c = ModifyKey(self.controller.key._key, change.value)
        # Just notify the dict of the change.
        self.controller.dict.binder.notify_change(c)


class _DictControllerDict(Bindable):
    def __init__(self, controller):
        self.controller = controller
    
    def get_value(self):
        raise SyntheticError
    
    def perform_change(self, change):
        if isinstance(change, LostValue):
            # Becoming synthetic. Tell the value that we're becoming synthetic,
            # and we're done.
            self.controller.value.binder.notify_change(LostValue())
        elif isinstance(change, (ModifyKey, DeleteKey)):
            if change.key != self.controller.key._key:
                # Change to a key that we're not looking at; do nothing
                return
            # Change to the key we're looking at
            if isinstance(change, ModifyKey):
                # Key has a value or changed values; notify the value of our
                # change
                self.controller.value.binder.notify_change(SetValue(change.value))
            else:
                # Key was deleted; set to the sentinel
                self.controller.value.binder.notify_change(SetValue(self.controller._sentinel))
        elif isinstance(change, SetValue):
            # Figure out what our own key is in the given dict, and notify the
            # value of it. Note that this could result in double notification
            # if we keep getting set to a new dict with our value the same both
            # times, but I don't think double notifications should be a problem
            # for now.
            # TODO: Split this into multiple statements, this line is way too
            # long
            self.controller.value.binder.notify_change(SetValue(change.value.get(self.controller.key._key, self.controller._sentinel)))
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
    
    def perform_change(self, change):
        @stm.atomically
        def _():
            if isinstance(change, InsertItem):
                MemoryList.perform_change(self, change)
                translated_change = InsertItem(change.index, self.from_function(change.item))
                MemoryList.perform_change(self.other, translated_change)
                self.other.binder.notify_change(translated_change)
            elif isinstance(change, ReplaceItem):
                MemoryList.perform_change(self, change)
                translated_change = ReplaceItem(change.index, self.from_function(change.item))
                MemoryList.perform_change(self.other, translated_change)
                self.other.binder.notify_change(translated_change)
            elif isinstance(change, DeleteItem):
                MemoryList.perform_change(self, change)
                MemoryList.perform_change(self.other, change)
                self.other.binder.notify_change(change)
            elif isinstance(change, SetValue):
                # TODO: Really ought to split this out for other widgets too lazy
                # to do any special processing to make use of
                for n in reversed(range(len(self.get_value()))):
                    self.perform_change(DeleteItem(n))
                for i, v in enumerate(change.value):
                    self.perform_change(InsertItem(i, v))
            else:
                raise TypeError


class ListTranslator(object):
    def __init__(self, a_to_b, b_to_a):
        self.a = _ListTranslatorList(a_to_b)
        self.b = _ListTranslatorList(b_to_a)
        self.a.other = self.b
        self.b.other = self.a


class _BinaryViewerParam(MemoryValue):
    def __init__(self, viewer, value):
        MemoryValue.__init__(self, value)
        self.viewer = viewer
    
    def perform_change(self, change):
        @stm.atomically
        def _():
            MemoryValue.perform_change(self, change)
            self.viewer.recalculate()


class _BinaryViewerOutput(MemoryValue):
    def __init__(self, viewer):
        MemoryValue.__init__(self, None)
        self.viewer = viewer
    
    def perform_change(self, change):
        raise Exception("Can't modify the output of a binary viewer")


class BinaryViewer(object):
    def __init__(self, function, a, b):
        self.function = function
        self.a = _BinaryViewerParam(self, a)
        self.b = _BinaryViewerParam(self, b)
        self.output = _BinaryViewerOutput(self)
        self.recalculate()
    
    def recalculate(self):
        @stm.atomically
        def _():
            a_value, b_value = self.a.value, self.b.value
            result = self.function(a_value, b_value)
            MemoryValue.perform_change(self.output, SetValue(result))
            self.output.binder.notify_change(SetValue(result))


class _DelayModel(PyValueMixin, SyntheticBindable):
    def __init__(self, controller):
        self.controller = controller
    
    def perform_change(self, change):
        if self.controller.synchronized.get():
            self.controller.view.binder.notify_change(change)


class _DelayView(PyValueMixin, SyntheticBindable):
    def __init__(self, controller):
        self.controller = controller
    
    def perform_change(self, change):
        self.controller.synchronized.binder.perform_change(SetValue(False))


class DelayController(object):
    def __init__(self):
        self.model = _DelayModel(self)
        self.view = _DelayView(self)
        self.synchronized = MemoryValue(True)
    
    def save(self):
        if self.synchronized.get():
            return
        else:
            @stm.atomically
            def _():
                self.model.binder.perform_change(SetValue(self.view.binder.get_value()))
                self.synchronized.binder.perform_change(SetValue(True))
    
    def cancel(self):
        if self.synchronized.get():
            return
        else:
            @stm.atomically
            def _():
                self.view.binder.perform_change(SetValue(self.model.binder.get_value()))
                self.synchronized.binder.perform_change(SetValue(True))


def key_bind(s, s_key, m, m_key, s_weak=True, m_weak=False):
    @stm.atomically
    def _():
        s_controller = DictController(s_key)
        m_controller = DictController(m_key)
        bind(m_controller.dict, m, s_weak, m_weak)
        bind(s_controller.dict, s, m_weak, s_weak)
        bind(s_controller.value, m_controller.value, s_weak, m_weak)

def v_key_bind(s, m, m_key, s_weak=True, m_weak=False):
    @stm.atomically
    def _():
        m_controller = DictController(m_key)
        bind(m_controller.dict, m, s_weak, m_weak)
        bind(s, m_controller.value, s_weak, m_weak)


def key_bind_v(s, s_key, m, s_weak=True, m_weak=False):
    @stm.atomically
    def _():
        s_controller = DictController(s_key)
        bind(s_controller.dict, s, m_weak, s_weak)
        bind(s_controller.value, m, s_weak, m_weak)











































    
    
