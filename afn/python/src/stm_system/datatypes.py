
from stm_system import stm

class Empty(Exception):
    pass


class _Item(object):
    def __init__(self, value):
        self.value = value
        self.next = stm.TVar()

class BroadcastQueue(object):
    def __init__(self):
        self._var = stm.TVar(stm.TVar(None))
    
    def put(self, value):
        item = _Item(value)
        self._var.get().set(item)
        self._var.set(item.next)
    
    def new_endpoint(self):
        return _BroadcastEndpoint(self._var.get())


class _BroadcastEndpoint(object):
    def __init__(self, var):
        self._var = stm.TVar(var)
    
    def get(self):
        if self._var.get().get() is None:
            stm.retry()
        else:
            item = self._var.get().get()
            self._var.set(item.next)
            return item.value


