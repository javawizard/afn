
from stm_system.avl import Node, empty as _empty, balance, pop_leftmost
from collections import MutableSequence
from stm_system import stm

def _list_insert(node, index, value):
    if node is _empty:
        # Empty, so just return a node with our value. TODO: Could check that
        # the index is == 0; if it's greater, then the original insertion index
        # is greater than the length of the list plus one, which doesn't make
        # sense.
        return Node(_empty, value, _empty)
    if index < node.left.weight: # Go left
        return balance(Node(_list_insert(node.left, index, value), node.value, node.right))
    elif index > node.left.weight: # Go right, but subtract left.weight + 1
        # from the index
        return balance(Node(node.left, node.value, _list_insert(node.right, index - 1 - node.left.weight, value)))
    else: # Node's supposed to be inserted here, so insert it into our left
        # child as the node at its weight'th index, or its last node
        return balance(Node(_list_insert(node.left, node.left.weight, value), node.value, node.right))            


def _list_replace(node, index, value):
    if node is _empty: # Walked right off the right end of the tree, so the
        # index is too high (or walked off the left end of the tree if the
        # index was less than 0)
        raise IndexError
    if index < node.left.weight: # Index is less than the number of children we
        # have on our left, so the item we're looking for is on our left side
        return Node(_list_replace(node.left, index, value), node.value, node.right)
    elif index > node.left.weight: # Same, but it's on our right side. Note
        # that we subtract the number of items on the left (that we're skipping)
        # as well as 1 for ourselves from the index before passing it down.
        return Node(node.left, node.value, _list_replace(node.right, index - node.left.weight - 1, value))
    else: # We're the node they're looking for
        return Node(node.left, value, node.right)


def _list_delete(node, index):
    if node is _empty:
        raise IndexError
    if index < node.left.weight: # Go left
        return balance(Node(_list_delete(node.left, index), node.value, node.right))
    elif index > node.left.weight: # Go right
        return balance(Node(node.left, node.value, _list_delete(node.right, index - node.left.weight - 1)))
    else: # We're supposed to delete this node
        if node.left is _empty and node.right is _empty:
            # No children; just return empty
            return _empty
        elif node.left is _empty:
            # Only a right child; return it
            return node.right
        elif node.right is _empty:
            # Only a left child; return it
            return node.left
        else:
            # Both a left and a right child, slightly more tricky. What we do
            # is pop the next item in the list and replace ourselves with it.
            new_value, new_right = pop_leftmost(node.right)
            return balance(Node(node.left, new_value, new_right))


def _list_get(node, index):
    if node is _empty:
        raise IndexError
    if index < node.left.weight: # Go left
        return _list_get(node.left, index)
    elif index > node.left.weight: # Go right
        return _list_get(node.right, index - node.left.weight - 1)
    else: # Found the node, so return its value
        return node.value


def _list_iter(node):
    if node is _empty:
        return
    for child in _list_iter(node.left):
        yield child
    yield node.value
    for child in _list_iter(node.right):
        yield child


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
        self.var = stm.TVar(_empty)
        if initial_values:
            # TODO: This could probably be made to be O(n) instead of
            # O(n log n) like it is right now
            self.extend(initial_values)
    
    def __getitem__(self, index):
        return _list_get(self.var.get(), index)
    
    def __setitem__(self, index, value):
        self.var.set(_list_replace(self.var.get(), index, value))
    
    def __delitem__(self, index):
        self.var.set(_list_delete(self.var.get(), index))
    
    def __len__(self):
        return self.var.get().weight
    
    def insert(self, index, value):
        self.var.set(_list_insert(self.var.get(), index, value))
    
    def __str__(self):
        return "TList(%r)" % stm.atomically(lambda: list(self))
    
    def __iter__(self):
        return _list_iter(self.var.get())
    
    __repr__ = __str__


