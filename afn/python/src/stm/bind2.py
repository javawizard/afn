
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
SyntheticError = None

class CustomBindable(Bindable):
    def __init__(self, perform_change_function=None, get_value_function=None):
        self.perform_change_function = perform_change_function
        self.get_value_function = get_value_function
    
    def get_value(self):
        if self.get_value_function:
            return self.get_value_function()
        else:
            raise SyntheticError
    
    def perform_change(self, change):
        if self.perform_change_function:
            self.perform_change_function(change)


class DictController(object):
    def __init__(self, key, sentinel=None):
        self._sentinel = sentinel
        self._actual_key = key
        self.key = CustomBindable(self._key_perform, self._key_get)
        self.value = CustomBindable(self._value_perform, self._value_get)
        self.dict = CustomBindable(self._dict_perform)
    
    def _dict_perform(self, change):
        if isinstance(change, ReplaceValue):
            # If we don't have a dict, use nothing. If we do but it doesn't
            # have our key, use the sentinel. If it does, use its value.
            old = nothing if change.old is nothing else change.old.get(self._actual_key, self._sentinel)
            new = nothing if change.new is nothing else change.new.get(self._actual_key, self._sentinel)
            self.value.binder.notify(ReplaceValue(old, new))
        elif change.key == self._actual_key: # ReplaceKey
            old = self._sentinel if change.old is nothing else change.old
            new = self._sentinel if change.new is nothing else change.new
            self.value.binder.notify(ReplaceValue(old, new))
    
    def _key_perform(self, change):
        if self.dict.binder.has_value:
            # Dict has a value, so notify our value of the value of the new
            # key in the dict
            dict_contents = self.dict.binder.get_value()
            old = dict_contents.get(change.old, self._sentinel)
            new = dict_contents.get(change.new, self._sentinel)
            self.value.binder.notify(ReplaceValue(old, new))
    
    def _key_get(self):
        return self._actual_key
    
    def _value_perform(self, change):
        if self.dict.binder.has_value:
            # Set the dict's key to the new value, translating _sentinel to
            # nothing to add/delete keys that are being set to the sentinel
            old = nothing if change.old == self._sentinel else change.old
            new = nothing if change.new == self._sentinel else change.new
            self.dict.binder.notify(ReplaceKey(self._actual_key, old, new))
    
    def _value_get(self):
        if self.dict.binder.has_value:
            # If the dict has a value, return the value for our key, or the
            # sentinel if the key doesn't exist
            return self.dict.binder.get_value().get(self._actual_key, self._sentinel)
        else:
            # No dict, so no value
            raise SyntheticError


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


















