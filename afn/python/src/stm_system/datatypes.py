
from stm_system import stm
from stm_system.avl import *

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
        
def list_insert(node, index, value):
    if isinstance(node, Empty):
        # Empty, so just return a node with our value. TODO: Could check that
        # the index is == 0; if it's greater, then the original insertion index
        # is greater than the length of the list plus one, which doesn't make
        # sense.
        return Node(empty, value, empty)
    if index < node.left.weight: # Go left
        return balance(Node(list_insert(node.left, index, value), node.value, node.right))
    elif index > node.left.weight: # Go right, but subtract left.weight + 1
        # from the index
        return balance(Node(node.left, node.value, list_insert(node.right, index - 1 - node.left.weight, value)))
    else: # Node's supposed to be inserted here
        return balance(Node(empty, value, node))


def list_replace(node, index, value):
    if isinstance(node, Empty):
        raise IndexError
    if index < node.left.weight:
        return Node(list_replace(node.left, index, value), node.value, node.right)
    elif index > node.left.weight:
        return Node(node.left, node.value, list_replace(node.right, index - node.left.weight - 1, value))
    else:
        return Node(node.left, value, node.right)


def list_get(node, index):
    if isinstance(node, Empty):
        raise IndexError
    if index < node.left.weight:
        return list_get(node.left, index)
    elif index > node.left.weight:
        return list_get(node.right, index - node.left.weight - 1)
    else:
        return node.value


class TList(object):
    def __init__(self):
        self.var = stm.TVar(empty)
    
    def __getitem__(self, index):
        return list_get(self.var.get(), index)
    
    def __setitem__(self, index, value):
        self.var.set(list_replace(self.var.get(), index, value))
    
    def __delitem__(self, index):
        raise NotImplementedError
    
    def __len__(self):
        return self.var.get().weight
    
    def insert(self, index, value):
        self.var.set(list_insert(self.var.get(), index, value))
    
    def __str__(self):
        return "TList(%r)" % list(self)


