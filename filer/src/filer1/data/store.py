
from abc import ABCMeta, abstractmethod

class Store(object):
    __metaclass__ = ABCMeta
    
    @abstractmethod
    def store(self, data):
        """
        Stores the specified commit, which is a BEC object, in this store. The
        BEC object in question must form a valid Filer commit; in particular,
        the store is permitted to rely on special keys like "parents" being
        present, and is free to throw an exception if they're not.
        """
    
    @abstractmethod
    def get(self, hash):
        """
        Returns the commit with the specified hash, or throws an
        exceptions.NoSuchObject if the commit in question does not exist.
        """
    
    @abstractmethod
    def has(self, hash):
        """
        Returns true if the specified hash has been stored in this store, false
        if it hasn't.
        """
    
    @abstractmethod
    def list(self):
        """
        Returns an iterator of some sort (could be a list, or the
        implementation of this method could just be a generator) providing all
        of the hashes stored in this store at the moment.
        """