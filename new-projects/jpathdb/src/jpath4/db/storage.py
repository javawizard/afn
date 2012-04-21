
from abc import ABCMeta, abstractmethod as abstract

class Storage(object):
    __metaclass__ = ABCMeta
    
    @abstract
    def apply_updates(self, list_of_updates):
        pass
    
    @abstract
    def get_root(self):
        pass
    
    @abstract
    def commit(self):
        pass
    
    @abstract
    def abort(self):
        pass
    
    @abstract
    def close(self):
        pass