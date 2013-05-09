
from stm_system import stm
from collections import MutableSet

class TSet(MutableSet):
    """
    A transactional mutable set.
    
    Right now, this actually just uses an underlying Python set which it copies
    on every write, so it's woefully inefficient. I'll be changing it to use
    the AVL trees that TList and TDict use soon, but I need to figure out how
    to properly implement weak sets in terms of them first. (I could always
    just have weak nodse pointing to dereferenced objects collected only during
    iteration, but then the iterators can't be completely isolated from the set
    itself, so that needs some thought.)
    """
    def __init__(self, initial_items=set(), backing_type=set):
        self.backing_type = backing_type
        self._var = stm.TVar(backing_type(initial_items))
    
    def __contains__(self, item):
        return self._var.get().__contains__(item)
    
    def __iter__(self):
        return self._var.get().__iter__()
    
    def __len__(self):
        return self._var.get().__len__()
    
    def add(self, item):
        new_set = self.backing_type(self._var.get())
        new_set.add(item)
        self._var.set(new_set)
    
    def discard(self, item):
        new_set = self.backing_type(self._var.get())
        new_set.discard(item)
        self._var.set(new_set)





