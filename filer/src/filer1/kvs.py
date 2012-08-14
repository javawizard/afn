
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
        cursor = self.db.cursor()
        cursor.execute("select key from keys")
        # Call fetchone until there's no results left
        for key in iter(cursor.fetchone, None):
            # Key will be a tuple; return its first (and only) item. TODO:
            # might want to use tuple unpacking as a sanity check that only one
            # item is returned.
            yield key[0]
    
    def close(self):
        self.db.close()
    
    def open(cls, in_file):
        with in_file.open("rb"):
            header = in_file.read(16)
        correct_header = "SQLite format 3\x00"
        if header != correct_header:
            raise exceptions.WrongKVSFormat(
                    "The header was %r, not %r as expected" % (
                            header, correct_header))
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


def open_store(in_file):
    """
    Opens the store located at the specified file.
    """
    # TODO: Make this a module-level constant, and update it when we add new
    # store types. And maybe have a module-level constant indicating the
    # default store type, too.
    known_types = [SQLiteKeyValueStore]
    for c in known_types:
        try:
            return c.open(in_file)
        except exceptions.WrongKVSFormat:
            continue
    raise exceptions.WrongKVSFormat("None of the known formats could open the "
            "store %r" % in_file.path)


# Make DefaultStore an alias for SQLiteKeyValueStore so that everything can
# later be switched over to use the hash table format once I get around to
# writing it. (Things can just use DefaultStore then, which will change to
# the HashTableStore thingie, or whatever I call it, once it's done.)
DefaultStore = SQLiteKeyValueStore


















