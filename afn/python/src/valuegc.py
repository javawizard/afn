
class Value(object):
    __slots__ = ["value"]
    def __init__(self, value):
        self.value = value
    
    def __str__(self):
        return "Value(%r)" % self.value
    
    __repr__ = __str__
    
    def __del__(self):
        print "Deleting %r" % self
