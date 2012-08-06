"""
BEC: Binary Encoding, Canonicalized: A JSON encoding, and corresponding
library, for storing JSON in a canonical format, and for allowing large amounts
of data (think gigabytes) to be stored in JSON objects without running out of
memory.
"""

from collections import MutableMapping, Mapping, Sequence
from cStringIO import StringIO
from filer1 import exceptions

JSON_TYPES = (int, long, float, basestring, bool, type(None), Sequence, Mapping)

class BECDict(object):
    def __init__(self):
        # Keys are dictionary keys. Values are ordinary values, or file-like
        # objects. (Note that strings are allowed for binary data, and will be
        # wrapped with a StringIO when requested as a stream.)
        self._data = {}
    
    def __setitem__(self, name, value):
        if not isinstance(name, basestring):
            raise exceptions.KeyType(name=name)
        if not isinstance(value, JSON_TYPES):
            raise exceptions.ValueType(value=value)
        self._data[name] = value
    
    def __delitem__(self, name):
        del self._data[name]
    
    def __getitem__(self, name):
        value = self._data[name]
        if hasattr(value, "read"): # file-like object; throw an exception for
            # now. Might want to check its size later and read it if it's not
            # too large.
            raise exceptions.LargeValue(value=value)
        # Not a file-like object; return as-is.
        return value
    
    def get_stream(self, name):
        value = self._data[name]
        if isinstance(value, basestring): # Return a StringIO wrapper
            return StringIO(value)
        if hasattr(value, "read"): # File-like object; return it as-is
            return value
    
    def set_stream(self, name, file):
        self._data[name] = file
    
    def __len__(self):
        return len(self._data)
    
    def __iter__(self):
        return self._data.__iter__()
    
    def __contains__(self, key):
        return self._data.__contains__(key)


def load(file, at_current=False):
    """
    Loads the specified BEC file. The file will be left seeked to just after
    the end of the data read.
    
    (Note that the file-like object passed in must support seek/tell.)
    
    If at_current is False (the default), the file will be seeked to the
    beginning before reading. If at_current is True, reading will begin at the
    current position. Setting at_current to True is useful for reading
    subsequent objects from a single BEC file; multiple objects can be written,
    one after the other, to the same file with bec.dump, and then loaded in
    turn with bec.load(..., at_current=True).
    """


def dump(value, file):
    """
    Writes the specified value to the specified BEC file-like object.
    """














