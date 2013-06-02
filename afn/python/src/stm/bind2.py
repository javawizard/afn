
from stm import atomically, TVar
from stm.tdict import TDict
from stm.tlist import TList
from collections import namedtuple

# Values of value bindables, keys and values of dict bindables, and values of
# list bindables must never have nothing as their value
nothing = 0
class NothingType(object):
    def __init__(self):
        if nothing != 0:
            raise Exception("NothingType is a singleton type; use "
                            "stm.bind2.nothing for the singleton instance.")
nothing = NothingType()

Bindable = None
ReplaceValue = namedtuple("ReplaceValue", ["old", "new"])
ReplaceKey = namedtuple("ReplaceKey", ["old", "new"])
ReplaceIndex = namedtuple("ReplaceIndex", ["old", "new"])
Value = None


class _DictControllerKey(Value):
    def __init__(self, controller):
        self.controller = controller
    
    def perform_change(self, change):
        Value.perform_change(self, change)
        if self.controller.dict.binder.has_value():
            old_value = self.controller.dict.binder.get_value().get(change.old, self.controller._sentinel)
            new_value = self.controller.dict.binder.get_value().get(change.new, self.controller._sentinel)
            self.controller.value.binder.notify_change(ReplaceValue(old_value, new_value))


class _DictControllerValue(Bindable):
    def __init__(self, controller):
        self.controller = controller
    
    def get_value(self):
        return self.controller.dict.binder.get_value().get(self.controller.key.get_value(), self.controller._sentinel)
    
    def perform_change(self, change):
        if self.controller.dict.binder.has_value():
            if change.old == self.controller._sentinel and change.new == self.controller._sentinel:
                # Nothing to change
                return
            elif change.old == self.controller._sentinel:
                # Key needs to be added
                change = ReplaceKey(self.controller.key.binder.get_value(), nothing, change.new)
            elif change.new == self.controller._sentinel:
                # Key needs to be deleted
                change = ReplaceKey(self.controller.key.binder.get_value(), change.old, nothing)
            else:
                # Key needs to be replaced
                change = ReplaceKey(self.controller.key.binder.get_value(), change.old, change.new)
            self.controller.dict.binder.notify_change(change)


class _DictControllerDict(Bindable):
    def __init__(self, controller):
        self.controller = controller
    
    def perform_change(self, change):
        # FIXME: Need to make sure the key we're looking at never becomes the
        # sentinel; need to check during AddKey/ReplaceKey and
        # GainedValue/ReplaceValue
        key = self.controller.key.binder.get_value()
        if isinstance(change, LostValue):
            self.controller.value.binder.notify_change(LostValue(change.old.get(key, self.controller._sentinel)))
        elif isinstance(change, GainedValue):
            self.controller.value.binder.notify_change(GainedValue(change.new.get(key, self.controller._sentinel)))
        elif isinstance(change, ReplaceValue):
            self.controller.value.binder.notify_change(ReplaceValue(change.old.get(key, self.controller._sentinel), change.new.get(key, self.controller._sentinel)))
        elif change.key == key:
            if isinstance(change, AddKey):
                self.controller.value.binder.notify_change(ReplaceValue(self.controller._sentinel, change.new))
            elif isinstance(change, ReplaceKey):
                self.controller.value.binder.notify_change(ReplaceValue(change.old, change.new))
            elif isinstance(change, DeleteKey):
                self.controller.value.binder.notify_change(ReplaceValue(change.old, self.controller._sentinel))


class DictController(object):
    def __init__(self, key, sentinel=None):
        self._sentinel = sentinel
        self.key = _DictControllerKey(self)
        self.value = _DictControllerValue(self)
        self.dict = _DictControllerDict(self)


class _ValueUnwrapperModel(Bindable):
    def __init__(self, controller, v_strong, m_strong):
        self.controller = controller
        self.v_strong = v_strong
        self.m_strong = m_strong
    
    def perform_change(self, change):
        if isinstance(change, (LostValue, ReplaceValue)):
            v_unbind_v(self.controller.view, change.old, self.v_strong, self.m_strong)
        if isinstance(change, (ReplaceValue, GainedValue)):
            v_bind_v(self.controller.view, change.new, self.v_strong, self.m_strong)


class ValueUnwrapper(object):
    def __init__(self, v_strong=False, m_strong=True):
        self.model = _ValueUnwrapperModel(self, v_strong, m_strong)
        self.view = SyntheticBindable()


def k_bind_k(v, v_key, m, m_key, v_strong=False, m_strong=True):
    v_controller = DictController(v_key)
    m_controller = DictController(m_key)
    bind(m_controller.dict, m, v_strong, m_strong)
    bind(v, v_controller.dict, v_strong, m_strong)
    bind(v_controller.value, m_controller.value, v_strong, m_strong)


def k_bind_v(v, v_key, m, v_strong=False, m_strong=True):
    v_controller = DictController(v_key)
    bind(v, v_controller.dict, v_strong, m_strong)
    bind(v_controller.value, m, v_strong, m_strong)


def v_bind_k(v, m, m_key, v_strong=False, m_strong=True):
    m_controller = DictController(m_key)
    bind(m_controller.dict, m, v_strong, m_strong)
    bind(v, m_controller.value, v_strong, m_strong)


def v_bind_v(v, m, v_strong=False, m_strong=True):
    bind(v, m, v_strong=False, m_strong=True)


class _ListControllerIndex(Value):
    def __init__(self, controller):
        self.controller = controller
    
    def perform_chance(self, change):
        


class ListController(object):
    def __init__(self, sentinel=None):
        self.index = _ListControllerIndex(self)
        self.value = _ListControllerValue(self)
        self.list = _ListControllerList(self)


















