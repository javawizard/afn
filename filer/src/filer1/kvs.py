
from abc import ABCMeta, abstractmethod
import sqlite3
from cStringIO import StringIO
from filer1 import exceptions

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
    
    @abstractmethod
    def close(self):
        """
        Closes this key-value store. All key-value stores are context managers;
        calling __exit__ is the same as calling close.
        """
    
    def __enter__(self):
        pass
    
    def __exit__(self, exc_type, exc_value, exc_traceback):
        self.close()
    
    @classmethod
    def open(cls, in_file):
        """
        Opens the key-value store at the specified file. Note that the file
        might not be in a format understood by this subclass; an
        exceptions.WrongKVSFormat should be thrown if that happens. Another
        type of Exception should be thrown if any other error occurs (such as
        a corrupt file).
        
        in_file is an instance of fileutils.File.
        
        The newly-opened key-value store will be returned.
        """
        raise NotImplementedError
    
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
        
        If this method throws an exception, the file may have been created but
        could be in an invalid state; it should be deleted if it exists.
        """
        raise NotImplementedError


class SQLiteKeyValueStore(KeyValueStore):
    def __init__(self, in_file):
        self.db = sqlite3.connect(in_file.path)
    
    def get(self, name):
        cursor = self.db.cursor()
        cursor.execute("select key, value from keys where key = ?",
                (name,))
        results = cursor.fetchone()
        if not results: # No such key
            raise exceptions.KVSKeyError(key=name)
        # Result will be a buffer because it's a blob; return it as an instance
        # of StringIO wrapping a corresponding str instance. TODO: Try just
        # passing the buffer straight to StringIO and see if it works.
        return StringIO(str(results[1]))
    
    def list(self):
        pass
    
    def open(cls, in_file):
        # FIXME: Check the format and raise WrongKVSFormat instead of whatever
        # error SQLite normally raises
        return SQLiteKeyValueStore(in_file)
    
    def create(cls, out_file, list_function, get_function):
        with sqlite3.connect(out_file.path) as db:
            cursor = db.cursor()
            cursor.execute("create table keys (key blob, value blob)")
            for name in list_function():
                with get_function(name) as value_file:
                    value = value_file.read()
                cursor.execute("insert into keys (key, value) values "
                        "(?, ?)", (name, value))


















