
from abc import ABCMeta, abstractmethod as abstract

class Storage(object):
    __metaclass__ = ABCMeta
    
    @abstract
    def create_dict(self):
        pass
    
    @abstract
    def create_list(self):
        pass
    
    @abstract
    def get_root(self):
        pass
    
    @abstract
    def commit(self):
        pass