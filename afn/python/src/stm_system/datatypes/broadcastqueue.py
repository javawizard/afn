
from stm_system import stm
from stm_system.datatypes import tobject

class Empty(Exception):
    pass


class _Item(object):
    def __init__(self, value):
        self.value = value
        self.next = stm.TVar()

class BroadcastQueue(tobject.TObject):
    def __init__(self):
        self._var = stm.TVar(None)
    
    def put(self, value):
        item = _Item(value)
        self._var.set(item)
        self._var = item.next
    
    def new_endpoint(self):
        return _BroadcastEndpoint(self._var)


class _BroadcastEndpoint(tobject.TObject):
    def __init__(self, var):
        self._var = var
    
    def get(self):
        if self._var.get() is None:
            stm.retry()
        else:
            item = self._var.get()
            self._var = item.next
            return item.value
        
