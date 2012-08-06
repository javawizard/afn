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

_DICT = "\x01"
_LIST = "\x02"
_STRING = "\x03"
_BYTES = "\x04"
_NUMBER = "\x05"
_BOOL = "\x06"
_NULL = "\x07"


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
        if whence == SEEK_SET: # Absolute seeking
            new_position = offset
        elif whence == SEEK_CUR: # Relative seeking
            new_position += offset
        else: # Assume SEEK_END; TODO: might want to check explicitly
            new_position = self.length + offset
        # Trim position to file boundaries
        new_position = max(min(new_position, self.length), 0)
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
    value_length = struct.unpack(">q", file.read(8))[0]
    if value_type == _NULL:
        # No bytes to read; just return None
        return None
    elif value_type == _BOOL:
        # Read one byte and return it as a bool
        return file.read(1) == "\x01"
    elif value_type == _NUMBER:
        # Read eight bytes and parse as an IEEE 754 double
        return struct.unpack(">d", file.read(8))[0]
    elif value_type == _STRING:
        # Read the string in, then decode using UTF-8
        return file.read(value_length).decode("UTF-8")
    elif value_type == _BYTES:
        # Create a stream for the bytes
        stream = BECStream(file, file.tell(), value_length)
        # Seek past the bytes of this file
        file.seek(value_length, SEEK_CUR)
        # Then return the stream
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
        result = {}
        while file.tell() < end:
            # We probably should check to make sure the key's a byte sequence,
            # but we're not going to for now, just because.
            # ONE IMPORTANT NOTE: I was previously doing this as a one-liner,
            # but I discovered that the right-hand side of an item assignment
            # is evaluated before the left-hand side, which resulted in the key
            # and the value getting flipped. Hence why this has to be done in
            # two lines.
            k = load_value(file)
            result[k] = load_value(file)
        # Return the result
        return result
    else:
        raise exceptions.InvalidTypeCode(type=value_type)


def dump_value(value, file):
    """
    Writes the specified value to the specified BEC file-like object.
    """
    if value is None:
        file.write(_NULL)
        file.write(struct.pack(">q", 0))
    elif isinstance(value, bool):
        file.write(_BOOL)
        file.write(struct.pack(">q", 1))
        file.write("\x01" if value else "\x00")
    elif isinstance(value, (int, long, float)):
        file.write(_NUMBER)
        file.write(struct.pack(">q", 8))
        file.write(struct.pack(">d", value))
    elif isinstance(value, basestring):
        file.write(_STRING)
        # Encode the string with UTF-8
        bytes = value.encode("UTF-8")
        # Write the length
        file.write(struct.pack(">q", len(bytes)))
        # Then write the bytes out 
        file.write(bytes)
    elif hasattr(value, "read"):
        file.write(_BYTES)
        # We won't know how many bytes we've written until after we've written
        # them, so skip past the size bytes and start writing. We'll come back
        # to them later.
        length_pos = file.tell()
        file.seek(8, SEEK_CUR)
        # Write bytes in 16KB blocks
        while True:
            data = value.read(16384)
            if not data: # No more data to read
                break
            # Write the data out
            file.write(data)
        # Figure out how much data there is; this is the current position minus
        # the length position, minus the eight bytes that the length itself
        # uses up
        length = file.tell() - length_pos - 8
        # Seek back to the length position and write the length out
        file.seek(length_pos)
        file.write(struct.pack(">q", length))
        # Then seek back over the data we wrote
        file.seek(length, SEEK_CUR)
    elif isinstance(value, Sequence):
        file.write(_LIST)
        # Same as with files, we won't know how much data we've written until
        # we've written everything out, so note where to write the length, then
        # skip over the length bytes
        length_pos = file.tell()
        file.seek(8, SEEK_CUR)
        # Write all the values out
        for v in value:
            dump_value(v, file)
        # Note how much data we wrote
        length = file.tell() - length_pos - 8
        # Seek back and write the length, then seek over the data we wrote
        file.seek(length_pos)
        file.write(struct.pack(">q", length))
        file.seek(length, SEEK_CUR)
    elif isinstance(value, Mapping):
        file.write(_DICT)
        # Same unknown length thing as with lists and files
        length_pos = file.tell()
        file.seek(8, SEEK_CUR)
        # Get a list of the keys
        keys = list(value.keys())
        # Make sure they're all strings
        for key in keys:
            if not isinstance(key, basestring):
                raise exceptions.KeyType(key=key)
        # Sort them all on their UTF-8 representation
        keys.sort(key=lambda k: k.encode("UTF-8"))
        # Then write out the keys and the values
        for k in keys:
            v = value[k]
            dump_value(k, file)
            dump_value(v, file)
        # Then write the length back out as with lists and files
        length = file.tell() - length_pos - 8
        file.seek(length_pos)
        file.write(struct.pack(">q", length))
        file.seek(length, SEEK_CUR)
    else:
        raise exceptions.ValueType(value=value)


# FIXME: Probably should use a header specifying the version of BEC being used,
# in case we change the format later on
dump = dump_value
load = load_value


def dumps(value):
    stream = StringIO()
    dump(value, stream)
    return stream.getvalue()


def loads(string):
    return load(StringIO(string))













