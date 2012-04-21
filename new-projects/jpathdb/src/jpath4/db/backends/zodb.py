"""
A JPathDB backend that uses a ZoDB object store to store its data.
"""

from persistent import Persistent
from persistent.list import PersistentList as ZPersistentList
from BTrees.OOBTree import OOBTree as ZOOBTree
from collections import MutableMapping, MutableSequence
import jpath4.db.storage
from jpath4.db.backends import standard_wrapper
from ZODB import FileStorage as ZFileStorage, DB as ZDB
import transaction


class DBDict(MutableMapping, Persistent):
    def __init__(self, source=None):
        self.oobtree = ZOOBTree()
        if source is not None:
            self.update(source) # TODO: update the oobtree instead, after
            # validating the types of the items in source
    
    def __getitem__(self, key):
        return self.oobtree[key]
    
    def __setitem__(self, key, value):
        if not isinstance(key, basestring):
            raise TypeError("Keys can only be strings")
        if not isinstance(value, (basestring, int, long, bool, type(None), DBDict, DBList)):
            raise TypeError("Values can only be those supported by the ZODB backend.")
        self.oobtree[key] = value
    
    def __delitem__(self, key):
        del self.oobtree[key]
    
    def __len__(self):
        return len(self.oobtree)
    
    def __iter__(self):
        return self.oobtree.__iter__()
    
    def __contains__(self, key):
        return key in self.oobtree


class DBList(MutableSequence, Persistent):
    def __init__(self, source=None):
        self.plist = ZPersistentList()
        if source is not None:
            self.extend(source) # TODO: extend the plist instead, after
            # validating the types of the items in source
    
    def __getitem__(self, index):
        return self.plist[index]
    
    def __setitem__(self, index, value):
        if not isinstance(value, (basestring, int, long, bool, type(None), DBDict, DBList)):
            raise TypeError("Values can only be those supported by the ZODB backend.")
        self.plist[index] = value
    
    def __delitem__(self, index):
        del self.plist[index]
    
    def insert(self, index, value):
        if not isinstance(value, (basestring, int, long, bool, type(None), DBDict, DBList)):
            raise TypeError("Values can only be those supported by the ZODB backend.")
        self.plist.insert(index, value)
    
    def __len__(self):
        return len(self.plist)
    
    def __iter__(self):
        return self.plist.__iter__()
    
    def __contains__(self, value):
        return self.plist.__contains__(value)


class DBStorage(jpath4.db.storage.Storage):
    def __init__(self, file_path):
        self.storage = ZFileStorage.FileStorage(file_path)
        self.db = ZDB(self.storage)
        self.connection = self.db.open()
        self.z_root = self.connection.root()
        if "jpath4-root" not in self.z_root:
            self.z_root["jpath4-root"] = DBDict()
            transaction.commit()
        self.unwrapped_root = self.z_root["jpath4-root"]
        self.wrapped_root = standard_wrapper.wrap(self.unwrapped_root)
    
    def get_root(self):
        return self.root
    
    def apply_updates(self, updates):
        standard_wrapper.apply_updates(self.wrapped_root, updates,
                classes={"object": DBDict, "list": DBList})
    
    def commit(self):
        transaction.commit()
    
    def abort(self):
        transaction.abort()
    
    def close(self):
        self.connection.close()
        self.db.close()
        self.storage.close()
        




















