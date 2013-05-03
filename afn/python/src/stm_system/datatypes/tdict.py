
from stm_system.avl import Node, empty, balance, pop_leftmost
from collections import MutableMapping
from stm_system import stm

# TODO: I'm pretty sure the first half or so of tdict and tlist can be merged
# together; I just need to figure out how to properly deal with the fact that
# tlist has to rewrite indexes when descending into right nodes. Perhaps have a
# function we pass to avl's hypothetical insert, delete, etc. functions that
# hand back whether we should go left, right, or whether we found a match or
# something, and also indicate a new "key" to use to recursively search. Needs
# some more thought.

def dict_insert(node, key, value):
    if node is empty:
        # Empty, so just return a node with our value.
        return Node(empty, (key, value), empty)
    if key < node.value[0]: # Key is less than this node's key, so go left
        return balance(Node(dict_insert(node.left, key, value), node.value, node.right))
    elif key > node.value[0]: # Key is greater than this node's key, so go right
        return balance(Node(node.left, node.value, dict_insert(node.right, key, value)))
    else: # Key is equal to this node's key, so just replace this node's value
        return Node(node.left, (key, value), node.right)            


def dict_delete(node, key):
    if node is empty:
        raise KeyError
    if key < node.value[0]:
        return balance(Node(dict_delete(node.left, key), node.value, node.right))
    elif key > node.value[0]:
        return balance(Node(node.left, node.value, dict_delete(node.right, key)))
    else: # We're supposed to delete this node
        if node.left is empty and node.right is empty:
            return empty
        elif node.left is empty:
            return node.right
        elif node.right is empty:
            return node.left
        else:
            new_value, new_right = pop_leftmost(node.right)
            return balance(Node(node.left, new_value, new_right))


def dict_get(node, key):
    if node is empty:
        raise KeyError
    if key < node.value[0]:
        return dict_get(node.left, key)
    elif key > node.value[0]:
        return dict_get(node.right, key)
    else:
        return node.value[1]


def dict_iter(node, value_slice):
    if node is empty:
        return
    for child in dict_iter(node.left):
        yield child
    yield node.value[value_slice]
    for child in dict_iter(node.right):
        yield child


class TDict(MutableMapping):
    def __init__(self):
        self.var = stm.TVar(empty)
    
    def __getitem__(self, key):
        return dict_get(self.var.get(), key)
    
    def __setitem__(self, key, value):
        self.var.set(dict_insert(self.var.get(), key, value))
    
    def __delitem__(self, key):
        self.var.set(dict_delete(self.var.get(), key))
    
    def __iter__(self):
        return dict_iter(self.var.get(), 0)
    
    def __len__(self):
        return self.var.get().weight
    
    def iterkeys(self):
        return iter(self)
    
    def keys(self):
        return list(self.iterkeys())
    
    def itervalues(self):
        return dict_iter(self.var.get(), 1)
    
    def values(self):
        return list(self.itervalues())
    
    def iteritems(self):
        return dict_iter(self.var.get(), slice(None))
    
    def items(self):
        return list(self.iteritems())


















