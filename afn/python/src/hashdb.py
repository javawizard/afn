

class HashDB(object):
    def get_type(self, hash):
        pass
    
    def get_value(self, hash, default=None):
        pass
    
    def get_values(self, hash):
        pass
    
    def get_value_count(self, hash):
        pass
    
    def get_value_range(self, hash, offset, length=1):
        pass
    
    def get_ref_count(self, hash):
        pass
    
    def get_ref_range(self, hash, offset, length=1):
        pass
    
    def lookup_ref(self, hash, key):
        pass
    
    def lookup_refs(self, hash, key):
        pass
    
    def delete(self, hash):
        pass
    
    def insert(self, value_iterable, ref_iterable):
        """
        Inserts a node. value_iterable should yield values to insert.
        ref_iterable should yield (key, hash) pairs.
        """
