
from nextgen.keys.observing import Observable
from nextgen.keys import changes as cc
from collections import OrderedDict
from afn.utils.singleton import Singleton as _Singleton
from afn.utils import slicer

_NONE = _Singleton("nextgen.keys.types._NONE")

class ObservableDict(Observable, dict):
    def _get_current_values(self, value_list):
        value_list += self.itervalues()
        super(ObservableDict, self)._get_current_values(value_list)
    
    def __delitem__(self, key):
        value = self[key]
        super(ObservableDict, self).__delitem__(key)
        self._notify_changed([cc.KeyRemoved(self, key, value)])
    
    def __setitem__(self, key, value):
        exists = key in self
        if exists:
            old = self[key]
        super(ObservableDict, self).__setitem__(key, value)
        if exists:
            self._notify_changed([cc.KeyUpdated(self, key, old, value)])
        else:
            self._notify_changed([cc.KeyAdded(self, key, value)])
    
    def clear(self):
        items = self.items()
        super(ObservableDict, self).clear()
        self._notify_changed([cc.KeyRemoved(self, k, v) for (k, v) in items])
    
    def copy(self):
        return ObservableDict(self.iteritems())
    
    def pop(self, key, default=_NONE):
        if key not in self:
            if default is _NONE:
                raise KeyError(key)
            else:
                return default
        value = self[key]
        del self[key] # This will call __delitem__, which will handle posting
        # the KeyRemoved event
        return value
    
    def popitem(self):
        if len(self) == 0:
            raise KeyError()
        key, value = self.iteritems().next()
        del self[key] # This will handle posting the KeyRemoved event
        return key, value
    
    def setdefault(self, key, default=None):
        if key not in self:
            self[key] = default # This will handle posting the KeyAdded event
            return default
        return self[key]
    
    def update(self, e, **f):
        changes = []
        if hasattr(e, "keys"):
            for key in e:
                value = e[key]
                if key in self:
                    changes.append(cc.KeyUpdated(self, key, self[key], value))
                else:
                    changes.append(cc.KeyAdded(self, key, value))
                super(ObservableDict, self).__setitem__(key, value)
        else:
            for key, value in e:
                if key in self:
                    changes.append(cc.KeyUpdated(self, key, self[key], value))
                else:
                    changes.append(cc.KeyAdded(self, key, value))
                super(ObservableDict, self).__setitem__(key, value)
        for key, value in f.iteritems():
            if key in self:
                changes.append(cc.KeyUpdated(self, key, self[key], value))
            else:
                changes.append(cc.KeyAdded(self, key, value))
            super(ObservableDict, self).__setitem__(key, value)
        self._notify_changed(changes)
    
    def __str__(self):
        return self.__repr__()
    
    def __repr__(self):
        return "<ObservableDict: %s>" % super(ObservableDict, self).__repr__()


class ObservableList(Observable, list):
    def _get_current_values(self, value_list):
        value_list += self.itervalues()
        super(ObservableDict, self)._get_current_values(value_list)
    
    # TODO: finish this up, and use afn.utils.slicer to implement slicing, and
    # see if Py3 has that and submit it as a patch if it doesn't











































