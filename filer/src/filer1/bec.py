"""
BEC: Binary Encoding, Canonicalized: A JSON encoding, and corresponding
library, for storing JSON in a canonical format, and for allowing large amounts
of data (think gigabytes) to be stored in JSON objects without running out of
memory.
"""

class RangeFile(object):
    def __init__(self, file, offset, length, read_only):
        self.file = file
        self.offset = offset
        self.length = length
        self.read_only = read_only
    
    def close(self):
        """
        Does nothing; close is a no-op on range files. The underlying file must
        be opened and closed separately.
        """
    
    def __enter__(self):
        """
        Method to allow RangeFile instances to be used as ordinary files; the
        implementation does nothing.
        """
    
    def __exit__(self, *args):
        """
        Ditto from __enter__. Does nothing; see self.close for why.
        """
    
    def __flush__(self, *args):
        """
        Flushes the underlying file.
        """
        self.file.flush()














