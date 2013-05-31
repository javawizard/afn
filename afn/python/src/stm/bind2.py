
from stm import atomically, TVar
from stm.tdict import TDict
from stm.tlist import TList

Bindable = None
LostValue = None
GainedValue = None
ReplaceValue = None
AddKey = None
ReplaceKey = None
DeleteKey = None
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
    
    def perform_change(self, change):
        if self.controller.dict.binder.has_value():
            if change.old == self.controller._sentinel and change.new == self.controller._sentinel:
                # Nothing to change
                return
            elif change.old == self.controller._sentinel:
                # Key needs to be added
                change = AddKey(self.controller.key.binder.get_value(), change.new)
            elif change.new == self.controller._sentinel:
                # Key needs to be deleted
                change = DeleteKey(self.controller.key.binder.get_value(), change.old)
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


class _ValueUnwrapperValue(Bindable):
    def __init__(self, controller):
        self.controller = controller
    
    def perform_change(self, change):
        if isinstance(change, (LostValue, ReplaceValue)):
            v_unbind_v(self.controller.bindable, change.old)
        if isinstance(change, (ReplaceValue, GainedValue)):
            v_bind_v(self.controller.bindable, change.new)


class ValueUnwrapper(object):
    def __init__(self):
        self.value = _ValueUnwrapperValue(self)
        self.bindable = SyntheticBindable()
















