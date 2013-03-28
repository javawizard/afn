
from abc import abstractmethod as abstract, ABCMeta as ABC
from collections import namedtuple
import collections

class SyntheticError(Exception):
    pass

class ValidationError(Exception):
    pass

SetValue = namedtuple("SetValue", ["value"])

ModifyDict = namedtuple("ModifyDict", ["changes"])
SetKey = namedtuple("SetKey", ["key", "value"])
DeleteKey = namedtuple("DeleteKey", ["name"])


class Bindable(object):
    """
    An object that can be bound.
    """
    
    def __init__(self):
        self._binder = None
    
    @abstract
    def get_value(self):
        """
        Gets and returns this bindable's value, or raises SyntheticError if
        this bindable is synthetic. Synthetic bindables are those which do not
        themselves store values, and instead rely on other bindables that
        they're bound to for value storage.
        
        A bindable's value is specific to what type of bindable it is. A more
        detailed explanation will be included soon.
        """
    
    def set_binder(self, binder):
        """
        Updates this bindable's binder to the specified binder. Note that this
        should not cause the bindable to update its value and such from the
        specified binder; it should only cause this bindable to issue future
        notifications about value changes to the specified binder when they
        happen.
        
        This may be called with None to indicate that a bindable is being
        unbound.
        """
        self._binder = binder
    
    def get_binder(self):
        """
        Gets the last binder set with set_binder.
        """
        return self._binder
    
    @abstract
    def receive_change(self, change):
        """
        """
    
    @abstract
    def validate_change(self, change):
        """
        """
    
    def post_change(self, change):
        if self._binder:
            self._binder.submit_change(change)
        else:
            self.validate_change(change)
            self.receive_change(change)
    
    @property
    def is_synthetic(self):
        try:
            self.get_value()
            return False
        except SyntheticError:
            return True


class Binder(object):
    """
    Binders bind bindables together and take care of marshalling changes and
    values among their bindables.
    
    Binder is a concrete class, and should not be subclassed. If you find
    yourself needing to subclass Binder, you're most likely doing it wrong.
    """
    def __init__(self):
        self.bindables = []
    
    def get_value(self):
        for b in self.bindables:
            try:
                return b.get_value()
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
    
    def submit_change(self, change):
        for b in self.bindables:
            b.validate_change(change)
        for b in self.bindables:
            b.receive_change(change)
    
    def bind(self, bindable):
        if bindable.get_binder() is not None:
            bindable.get_binder().unbind(bindable)
        if self.is_synthetic and bindable.is_synthetic:
            # No concrete bindables yet; link things up but don't post any
            # SetValue changes yet
            bindable.set_binder(self)
            self.bindables.append(bindable)
        elif self.is_synthetic and not bindable.is_synthetic:
            # We're synthetic but about to become concrete. Link things up,
            # then send around the specified bindable's value.
            bindable.set_binder(self)
            self.bindables.append(bindable)
            try:
                # TODO: This submits back to the bindable itself. Consider
                # having two lists, one of concrete bindables that we get
                # values from and one of bindables to notify of changes, then
                # add the bindable to the former before sending the change
                # around but not to the latter.
                self.submit_change(SetValue(bindable.get_value()))
            except ValidationError as e:
                bindable.set_binder(None)
                self.bindables.remove(bindable)
                raise
        else:
            # We're already concrete; link things up, then post our current
            # value to the specified bindable.
            # TODO: Might need to link things up before validating, in case
            # validation needs to consult our value
            bindable.validate_change(SetValue(self.get_value()))
            bindable.set_binder(self)
            self.bindables.append(bindable)
            bindable.receive_change(SetValue(self.get_value()))
    
    def unbind(self, bindable):
        if bindable.get_binder() == self:
            self.bindables.remove(bindable)
            bindable.set_binder(None)
        else:
            raise Exception("Not bound to the specified bindable")


class Value(Bindable):
    def __init__(self, value, validator=lambda s: True):
        Bindable.__init__(self)
        self._value = value
        self.validator = validator
    
    @property
    def value(self):
        return self._value
    
    @value.setter
    def value(self, value):
        self.post_change(SetValue(value))
    
    def get_value(self):
        return self._value
    
    def receive_change(self, change):
        self._value = change.value
    
    def validate_change(self, change):
        if isinstance(change, SetValue):
            self.validator(change.value)
        else:
            raise ValidationError


class Dict(Bindable, collections.MutableMapping):
    def __init__(self):
        Bindable.__init__(self)
        self._dict = {}
    
    def get_value(self):
        return self._dict
    
    def validate_change(self, change):
        if not isinstance(change, (SetValue, ModifyDict)):
            raise ValidationError
    
    def receive_change(self, change):
        if isinstance(change, ModifyDict):
            changes = change.changes
            for c in changes:
                if isinstance(c, SetKey):
                    self._dict[c.key] = c.value
                elif isinstance(c, DeleteKey):
                    del self._dict[c.key]
        else:
            self._dict = {}
            for k, v in change.value.iteritems():
                self._dict[k] = v
    
    # TODO: Could consider splitting off the storage backend and the
    # MutableMapping backend into separate pieces, with the former implementing
    # just Bindable and the latter being a synthetic translator between the
    # bindings system and MutableMapping's functions
    
    def __setitem__(self, key, value):
        self.post_change(ModifyDict(changes=[SetKey(key, value)]))
    
    def __delitem__(self, key):
        self.post_change(ModifyDict(changes=[DeleteKey(key)]))
    
    def __getitem__(self, key):
        return self._dict.__getitem__(key)
    
    def __len__(self):
        return self._dict.__len__()
    
    def __iter__(self):
        return self._dict.__iter__()
    
    def __contains__(self, key):
        return self._dict.__contains__(key)











































