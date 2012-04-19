"""
A JPathDB backend that uses a ZoDB object store to store its data.
"""

from persistent import Persistent
from persistent.list import PersistentList as ZPersistentList
from BTrees.OOBTree import OOBTree


class StoredDict(Persistent):
    def __init__(self):
        self.data = OOBTree()
    
    def set(self, key, value):
        self.data[key] = value
    
    def remove(self, key):
        del self.data[key]
    
    def iterkeys


class StoredList(Persistent):
    def __init__(self):
        self.data = ZPersistentList()


class StoredNumber(Persistent):
    pass


class StoredBoolean(Persistent):
    pass


class StoredNull(Persistent):
    pass


class StoredString(Persistent):
    pass
