"""
BEC: Binary Encoding, Canonicalized: A JSON encoding, and corresponding
library, for storing JSON in a canonical format, and for allowing large amounts
of data (think gigabytes) to be stored in JSON objects without running out of
memory.
"""

from collections import MutableMapping, Mapping, Sequence
from cStringIO import StringIO
from filer1 import exceptions
from os import SEEK_SET, SEEK_CUR, SEEK_END
import struct

JSON_TYPES = (int, long, float, basestring, bool, type(None), Sequence, Mapping)

_DICT = 1
_LIST = 2
_BYTES = 3
_NUMBER = 4
_BOOL = 5
_NULL = 6


class BECDict(object):
    def __init__(self):
        # Keys are dictionary keys. Values are ordinary values, or file-like
        # objects. (Note that strings are allowed for binary data, and will be
        # wrapped with a StringIO when requested as a stream.)
        self._data = {}
    
    def __setitem__(self, name, value):
        if not isinstance(name, basestring):
            raise exceptions.KeyType(name=name)
        if not isinstance(value, JSON_TYPES) and not hasattr(value, "read"):
            # We're allowing setting file-like objects here to make the logic
            # for reading BEC files simpler. TODO: is this a good idea?
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


class BECStream(object):
    def __init__(self, file, offset, length):
        self.file = file
        self.offset = offset
        self.length = length
        self.position = 0
    
    def close(self):
        pass
    
    def flush(self):
        pass
    
    def next(self):
        raise NotImplementedError
    
    def read(self, size=None):
        # Seek to our current position
        self.file.seek(self.offset + self.position)
        # Figure out how many bytes left before the end of the file
        remaining = self.length - self.position
        # Trim the size down to the maximum bytes remaining
        if size is None or size > remaining:
            size = remaining
        # Add the number of bytes to our position
        self.position += size
        # Then read and return the bytes.
        return self.file.read(size)
    
    def seek(self, offset, whence=SEEK_SET):
        new_position = self.position
        if whence == SEEK_SET: # Absolute seeking; add self.offset first
            new_position = self.offset + offset
        elif whence == SEEK_CUR: # Relative seeking
            new_position += offset
        else: # Assume SEEK_END; TODO: might want to check explicitly
            new_position = self.offset + self.length + offset
        # Trim position to file boundaries
        new_position = max(min(new_position, self.offset + self.length), self.offset)
        # Then update our position
        self.position = new_position
    
    def tell(self):
        return self.position
    
    # TODO: need to implement readline, readlines, and next


def load_value(file):
    """
    Loads the specified BEC file.
    """
    # First byte is type, next eight bytes are length
    value_type = file.read(1)
    value_length = struct.unpack("q", file.read(8))
    if value_type == _NULL:
        # No bytes to read; just return None
        return None
    elif value_type == _BOOL:
        # Read one byte and return it as a bool
        return file.read(1) == "\x01"
    elif value_type == _NUMBER:
        # Read eight bytes and parse as an IEEE 754 double
        return struct.unpack("d", file.read(8))[0]
    elif value_type == _BYTES:
        # If length is less than 256 bytes, read it into memory and return it.
        if value_length < 256:
            return file.read(value_length)
        # Otherwise, create a BECStream and return it, then seek past the bytes
        # for now.
        else:
            stream = BECStream(file, file.tell(), value_length)
            file.seek(value_length, SEEK_CUR)
            return stream
    elif value_type == _LIST:
        # Store the point at which the list should end
        end = file.tell() + value_length
        # Read values until we hit the end position
        result = []
        while file.tell() < end:
            result.append(load_value(file))
        # Return the list
        return result
    elif value_type == _DICT:
        # Store the point at which the dictionary should end
        end = file.tell() + value_length
        # Read keys and values until we hit the end position
        result = BECDict()
        while file.tell() < end:
            # We probably should check to make sure the key's a byte sequence,
            # but we're not going to for now, just because.
            result[load_value(file)] = load_value(file)
        # Return the result
        return result
    else:
        raise exceptions.InvalidTypeCode(type=value_type)


def dump(value, file):
    """
    Writes the specified value to the specified BEC file-like object.
    """














