
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
        self._v_strong = v_strong
        self._m_strong = m_strong
        self.model = CustomBindable(self._perform)
        self.view = CustomBindable()
    
    def _perform(self, change):
        # Just unbind from the old value if we have one, then bind to the new
        if change.old is not nothing:
            v_unbind_v(self.view, change.old, self._v_strong, self._m_strong)
        if change.new is not nothing:
            v_bind_v(self.view, change.old, self._v_strong, self._m_strong)


def model_key(model, key):
    """
    Returns a value viewing the specified key of the specified model, which
    should be a dictionary of some sort. The returned value holds a strong
    reference to the model, but not the other way around.
    """
    return key_as_value(model, key, True, False)


def view_key(view, key):
    """
    Returns a value viewing the specified key of the specified view, which
    should be a dictionary of some sort. The passed in view will hold a strong
    reference to the returned value, but not the other way around.
    """
    return key_as_value(view, key, False, True)


def key_as_value(dictionary, key, dict_strong, value_strong):
    """
    Returns a value viewing the specified key of the specified dictionary. If
    dict_strong is True, the returned value will hold a strong reference to the
    specified dictionary. If value_strong is True, the specified dictionary
    will (once this call returns) hold a strong reference to the returned
    value.
    """
    controller = DictController(key)
    v_bind_v(controller.dict, dictionary, value_strong, dict_strong)
    return controller.value    


def k_bind_k(v, v_key, m, m_key, v_strong=False, m_strong=True):
    v_value = key_as_value(v, v_key, v_strong, m_strong)
    m_value = key_as_value(m, m_key, m_strong, v_strong)
    v_bind_v(v_value, m_value, v_strong, m_strong)


def k_bind_v(v, v_key, m, v_strong=False, m_strong=True):
    v_bind_v(key_as_value(v, v_key, v_strong, m_strong), m, v_strong, m_strong)


def v_bind_k(v, m, m_key, v_strong=False, m_strong=True):
    v_bind_v(v, key_as_value(m, m_key, m_strong, v_strong), v_strong, m_strong)


def v_bind_v(v, m, v_strong=False, m_strong=True):
    bind(v, m, v_strong, m_strong)


class BinaryCombinator(Bindable):
    def __init__(self, a=None, b=None):
        self._value = nothing
        self.a = CustomBindable(self._component_perform)
        self.b = CustomBindable(self._component_perform)
        if a:
            v_bind_v(self.a, a)
        if b:
            v_bind_v(self.b, b)
        self._recompute()
    
    def _compute(self, a_value, b_value):
        raise NotImplementedError
    
    def get_value(self):
        return self._value
    
    def perform_change(self, change):
        raise Exception("Binary combinators' outputs cannot be modified")
    
    def _component_perform(self, change):
        old = self._value
        self._recompute()
        self.binder.notify(ReplaceValue(old, self._value))
    
    def _recompute(self):
        try:
            a_value = self.a.binder.get_value()
            b_value = self.b.binder.get_value()
        except SyntheticError:
            self._value = nothing
            return
        self._value = self._compute(a_value, b_value)


class UnaryCombinator(Bindable):
    def __init__(self, a=None):
        self._value = nothing
        self.a = CustomBindable(self._component_perform)
        if a:
            v_bind_v(self.a, a)
        self._recompute()
    
    def _compute(self, a_value):
        raise NotImplementedError
    
    def get_value(self):
        return self._value
    
    def perform_change(self):
        raise Exception("Unary combinators' outputs cannot be modified")
    
    def _component_perform(self, change):
        old = self._value
        self._recompute()
        self.binder.notify(ReplaceValue(old, self._value))
    
    def _recompute(self):
        try:
            a_value = self.a.binder.get_value()
        except SyntheticError:
            self._value = nothing
            return
        self._value = self._compute(a_value)


class And(BinaryCombinator):
    def _compute(self, a, b):
        return a and b


class Or(BinaryCombinator):
    def _compute(self, a, b):
        return a or b


class Not(UnaryCombinator):
    def _compute(self, a):
        return not a


class Add(BinaryCombinator):
    def _compute(self, a, b):
        return a + b


class Subtract(BinaryCombinator):
    def _compute(self, a, b):
        return a - b


class Multiply(BinaryCombinator):
    def _compute(self, a, b):
        return a * b


class Divide(BinaryCombinator):
    def _compute(self, a, b):
        return a / b


class Translate(UnaryCombinator):
    def __init__(self, function, a):
        UnaryCombinator.__init__(a)
        self._function = function
    
    def _compute(self, a):
        return self._function(a)





















