
from abc import ABCMeta, abstractmethod

class KeyValueStore(object):
    __metaclass__ = ABCMeta
    
    @abstractmethod
    def get(self, name):
        """
        Gets a file-like object that can be used to read the value of the key
        with the specified name.
        """
    
    @abstractmethod
    def list(self):
        """
        Returns a generator over the names of the keys in this store.
        """
    
    @classmethod
    def open(cls, in_file):
        """
        Opens the key-value store at the specified file. Note that the file
        might not be in a format understood by this subclass; an
        exceptions.WrongKVSFormat should be thrown if that happens. Another
        type of Exception should be thrown if any other error occurs (such as
        a corrupt file).
        
        in_file is an instance of fileutils.File.
        """
    
    @classmethod
    def create(cls, out_file, list_function, get_function):
        """
        Creates a new key-value store. The store will be written to the
        specified fileutils.File instance, which should not already exist.
        list_function and get_function are functions with the same signature as
        self.list and self.get; they will be used to build the new key-value
        store. A key-value store could be built off of the files in a folder
        with something like:
        
            create(out_file, lambda: (f.path for f in the_folder.list()),
                   lambda n: the_folder.child(n).open("rb"))
        
        Note that the file-like objects returned from get_function will always
        be closed after they are used, which is in line with how self.open
        should work.
        """


