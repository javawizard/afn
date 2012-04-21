
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



















