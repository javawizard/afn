
from abc import ABCMeta as ABC, abstractmethod as abstract
import weakref

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


def validate_bind(binder_a, binder_b):
    pass


def perform_bind(binder_a, binder_b, strong_a, strong_b):
    pass