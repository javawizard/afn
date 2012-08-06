
from abc import ABCMeta, abstractmethod

class Store(object):
    __metaclass__ = ABCMeta
    
    @abstractmethod
    def store(self, data):
        """
        Stores the specified BEC value in this store. The hash of the data, as
        a hexidecimal string, will be returned.
        """
    
    @abstractmethod
    def get(self, hash):
        """
        Returns the BEC object with the specified hash previously stored in
        this store, or throws an exceptions.NoSuchObject if no such object has
        been added to this store.
        """