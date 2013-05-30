
from stm import avl
from collections import MutableSequence
import stm

def _selector(node, index):
    if index < node.left.weight: # Index is less than the number of children
        # the left node has, so go left
        return avl.LEFT, index
    elif index > node.left.weight: # Index is greater than the number of
        # children the left node has, so go right, but subtract the left node's
        # weight as well as 1 (for our own value) so that we're speaking in
        # terms of indexes as our right node would know them
        return avl.RIGHT, index - 1 - node.left.weight
    else: # Index is equal to the number of children the left node has, so it
        # refers to this node
        return avl.STOP


class TList(MutableSequence):
    """
    A transactional list.
    
    Internally, transactional lists are maintained with a single TVar holding a
    copy-on-write binary tree annotated with list indexes. Insertion (and
    appending), removal, and lookup are therefore all O(log n) operations.
    len() is O(1), as is iter().
    
    One nice property of using a copy-on-write binary tree is iteration: the
    iterator returned from iter(tlist) is a snapshot of the list at that point
    in time. The list can therefore be safely modified during iteration,
    without affecting the items produced by the iteration.
    
    All of TList's functions must be called within an STM transaction, with the
    exception of __str__/__repr__, which, for the sake of convenience,
    wrap themselves in a call to stm.atomically() internally. 
    """
    def __init__(self, initial_values=[]):
        self.var = stm.TVar(avl.empty)
        if initial_values:
            # Optimize to O(1) if we're cloning another TList
            if isinstance(initial_values, TList):
                self.var.set(initial_values.var.get())
            else:
                # TODO: This could probably be made to be O(n) instead of
                # O(n log n) like it is right now
                self.extend(initial_values)
    
    def __getitem__(self, index):
        return avl.get(self.var.get(), _selector, index, lambda: IndexError(index))
    
    def __setitem__(self, index, value):
        self.var.set(avl.insert(self.var.get(), _selector, index, value, lambda: IndexError(index), True, False))
    
    def __delitem__(self, index):
        self.var.set(avl.delete(self.var.get(), _selector, index, lambda: IndexError(index)))
    
    def __len__(self):
        return self.var.get().weight
    
    def insert(self, index, value):
        self.var.set(avl.insert(self.var.get(), _selector, index, value, lambda: IndexError(index), False, True))
    
    def __str__(self):
        return "TList(%r)" % stm.atomically(lambda: list(self))
    
    def __iter__(self):
        return avl.traverse(self.var.get())
    
    __repr__ = __str__


