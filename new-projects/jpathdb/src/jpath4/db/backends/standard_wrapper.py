
from jpath4.query import data as d
import collections as c


class ListWrapper(d.List):
    def __init__(self, py_list):
        self.py_list = py_list
    
    def get_size(self):
        return len(self.py_list)
    
    def get_item(self, index):
        return wrap(self.py_list[index])
    
    def get_items(self):
        return ListWrapperSequence(self)


class ListWrapperSequence(d.Sequence):
    def __init__(self, list_wrapper):
        self.list_wrapper = list_wrapper
    
    def get_item(self, index):
        return self.list_wrapper.get_item(index)
    
    def get_size(self):
        return self.list_wrapper.get_size()
    
    def is_synthetic(self):
        # TODO: What do we choose for this? Perhaps see if the underlying
        # list_wrapper's py_list is actually a list or a tuple and return true
        # if it is, false if it isn't, or see if the underlying value implements
        # an (empty) abstract class called Synthetic that we define, or something.
        return False


class ObjectWrapper(d.Object):
    def __init__(self, py_dict):
        self.py_dict = py_dict
    
    def get_value(self, key):
        # TODO: Only string keys are supported right now; consider allowing
        # other types in case the underlying implementation wants to permit them
        if isinstance(key, d.String):
            string_key = key.get_value()
            if string_key in self.py_dict:
                return wrap(self.py_dict[string_key]) 
        return None
    
    def get_pair(self, key):
        # TODO: There /has/ to be a better way than this... Maybe write an
        # ObjectWrapperPair class that dynamically fetches the key and value of
        # the pair or something...
        value = self.get_value(key)
        if not value:
            return None
        return d.StandardPair(key, value)
    
    def get_size(self):
        return len(self.py_dict)
    
    def get_values(self):
        # TODO: write a thing similar to ListWrapperSequence for values
        return d.StandardSequence([wrap(v) for v in self.py_dict.itervalues()])
    
    def get_pairs(self):
        # TODO: write a thing similar to ListWrapperSequence for pairs
        return d.StandardSequence([d.StandardPair(wrap(k), wrap(v)) for k, v in self.py_dict.iteritems()])


def wrap(value):
    # TODO: add a weak map to avoid creating multiple wrappers for the same value
    if isinstance(value, bool):
        return d.StandardBoolean(value)
    elif isinstance(value, (int, long, float)):
        return d.StandardNumber(value)
    elif value is None:
        return d.StandardNull()
    elif isinstance(value, basestring):
        return d.StandardString(value)
    elif isinstance(value, c.Mapping):
        return ObjectWrapper(value)
    elif isinstance(value, c.Sequence):
        return ListWrapper(value)
    else:
        raise Exception("Cannot wrap value " + repr(value) + " of type " + repr(type(value)))



















