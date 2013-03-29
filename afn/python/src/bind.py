
from abc import ABCMeta as ABC, abstractmethod as abstract
import weakref
from collections import namedtuple
import collections

class SyntheticError(Exception):
    pass

class ValidationError(Exception):
    pass

SetValue = namedtuple("SetValue", ["value"])
SyntheticValue = namedtuple("SyntheticValue", [])

ModifyDict = namedtuple("ModifyDict", ["changes"])
SetKey = namedtuple("SetKey", ["key", "value"])
DeleteKey = namedtuple("DeleteKey", ["name"])


class Bindable(object):
    __metaclass__ = ABC
    
    @abstract
    def get_value(self):
        pass
    
    @abstract
    def validate_change(self, change):
        pass
    
    @abstract
    def perform_change(self, change):
        pass
    
    @property
    def is_synthetic(self):
        try:
            self.get_value()
            return False
        except SyntheticError:
            return True


class Binder(object):
    def __init__(self, bindable):
        self.bindable = bindable
        self.binders = []
    
    def validate_change(self, change, skip_self=False, visited=None):
        if visited is None:
            visited = []
        if self in visited:
            return
        visited.append(self)
        if not skip_self:
            self.bindable.validate_change(change)
        for binder in self.binders:
            binder.validate_change(change, visited=visited)
    
    def perform_change(self, change, skip_self=False, visited=None):
        if visited is None:
            visited = []
        if self in visited:
            return
        visited.append(self)
        if not skip_self:
            self.bindable.perform_change(change)
        for binder in self.binders:
            binder.perform_change(change, visited=visited)
    
    def get_value(self, skip_self=False, visited=None):
        if visited is None:
            visited = []
        if self in visited:
            raise SyntheticError
        visited.append(self)
        if not skip_self:
            try:
                return self.bindable.get_value()
            except SyntheticError:
                pass
        for binder in self.binders:
            try:
                return binder.get_value(visited=visited)
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
    
    def validate_bind(self, other):
        pass
    
    def perform_bind(self, other, self_strong, other_strong):
        pass





