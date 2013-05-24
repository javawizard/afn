
from stm import tdict

class TObject(object):
    """
    An abstract class that causes all of its subclass's attributes to be backed
    by a TDict. This results in the subclass's attributes being transactional,
    so that they can be modified during a transaction without having to
    explicitly wrap all of them with TVars.
    """
    def __init__(self):
        object.__setattr__(self, "_tobject_dict", tdict.TDict())
    
    def __getattr__(self, name):
        if name == "_tobject_dict":
            return object.__getattribute__(self, name)
        return self._tobject_dict[name]
    
    def __setattr__(self, name, value):
        self._tobject_dict[name] = value
    
    def __delattr__(self, name):
        del self._tobject_dict[name]
    
    def __dir__(self):
        return self._tobject_dict.keys()


