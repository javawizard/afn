
from stm_system.datatypes import tdict

class TObject(object):
    def __init__(self):
        object.__setattr__(self, "_dict", tdict.TDict())
    
    def __getattr__(self, name):
        return self._dict[name]
    
    def __setattr__(self, name, value):
        self._dict[name] = value
    
    def __delattr__(self, name):
        del self._dict[name]
    
    def __dir__(self):
        return self._dict.keys()


