
from abc import ABCMeta as ABC, abstractmethod as abstract
from afn.utils import singleton

_NO_VALUE = singleton.Singleton("nextgen.keys2.observing._NO_VALUE")

class KeyObservable(object):
    __metaclass__ = ABC
    
    def observe_path(self, observer, path, ):
        raise NotImplementedError


class ObservableObject(KeyObservable):
    def __setattr__(self, name, value):
        try:
            old = object.__getattribute__(self, name, value)
        except AttributeError: # Attribute doesn't exist yet
            old = _NO_VALUE
        object.__setattr__(self, name, value)
        if old is _NO_VALUE:
            self._notify_key_added(name, value)
        else:
            self._notify_key_changed(name, old, value)
    
    def __delattr__(self, name, value):
        old = object.__getattribute__(self, name, value)
        self._notify_key_removed(name, old)
    
    def set_value_for_key(self, key, value):
        setattr(self, key, value)
    
    def get_value_for_key(self, key):
        return getattr(self, key)



