
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
        # Empty (we never found a matching value, so the specified value isn't
        # already present), so we just return a new value containing the value
        # we were called with.
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
    if key < node.value[0]: # Go left
        return balance(Node(dict_delete(node.left, key), node.value, node.right))
    elif key > node.value[0]: # Go right
        return balance(Node(node.left, node.value, dict_delete(node.right, key)))
    else: # We're supposed to delete this node. The logic here is identical to
        # that of tlist.list_delete; see that function's documentation for
        # info and comments on how this one's works.
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
    if key < node.value[0]: # Go left
        return dict_get(node.left, key)
    elif key > node.value[0]: # Go right
        return dict_get(node.right, key)
    else: # Found the key; just return the value part of its tuple
        return node.value[1]


def dict_iter(node, value_slice):
    if node is empty:
        return
    for child in dict_iter(node.left, value_slice):
        yield child
    yield node.value[value_slice]
    for child in dict_iter(node.right, value_slice):
        yield child


class TDict(MutableMapping):
    """
    A transactional list.
    
    Internally, transactional dicts are maintained with a single TVar holding a
    copy-on-write binary tree annotated with dict keys. Insertion (and
    appending), removal, and lookup are therefore all O(log n) operations.
    len() is O(1), as is iter(), iterkeys(), iteritems(), and itervalues().
    
    One nice property of using a copy-on-write binary tree is iteration: the
    iterator returned from iter(tdict) is a snapshot of the dict's keys at that
    point in time. The dict can therefore be safely modified during iteration,
    without affecting the keys produced by the iteration. The same is, of
    course, true of iterkeys, iteritems, and itervalues.
    
    All of TDict's functions must be called within an STM transaction, with the
    exception of __str__/__repr__, which, for the sake of convenience,
    wrap themselves in a call to stm.atomically() internally. 
    """
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
    
    def __str__(self):
        return "TDict(%r)" % stm.atomically(lambda: dict(self))


















