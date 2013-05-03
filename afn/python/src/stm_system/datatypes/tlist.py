
from stm_system.avl import Node, empty, balance, pop_leftmost
from collections import MutableSequence
from stm_system import stm

def list_insert(node, index, value):
    if node is empty:
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
    else: # Node's supposed to be inserted here, so insert it into our left
        # child as the node at its weight'th index, or its last node
        return balance(Node(list_insert(node.left, node.left.weight, value), node.value, node.right))            


def list_replace(node, index, value):
    if node is empty:
        raise IndexError
    if index < node.left.weight:
        return Node(list_replace(node.left, index, value), node.value, node.right)
    elif index > node.left.weight:
        return Node(node.left, node.value, list_replace(node.right, index - node.left.weight - 1, value))
    else:
        return Node(node.left, value, node.right)


def list_delete(node, index):
    if node is empty:
        raise IndexError
    if index < node.left.weight:
        return balance(Node(list_delete(node.left, index), node.value, node.right))
    elif index > node.left.weight:
        return balance(Node(node.left, node.value, list_delete(node.right, index - node.left.weight - 1)))
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


def list_get(node, index):
    if node is empty:
        raise IndexError
    if index < node.left.weight:
        return list_get(node.left, index)
    elif index > node.left.weight:
        return list_get(node.right, index - node.left.weight - 1)
    else:
        return node.value


def list_iter(node):
    if node is empty:
        return
    for child in list_iter(node.left):
        yield child
    yield node.value
    for child in list_iter(node.right):
        yield child


class TList(MutableSequence):
    def __init__(self):
        self.var = stm.TVar(empty)
    
    def __getitem__(self, index):
        return list_get(self.var.get(), index)
    
    def __setitem__(self, index, value):
        self.var.set(list_replace(self.var.get(), index, value))
    
    def __delitem__(self, index):
        self.var.set(list_delete(self.var.get(), index))
    
    def __len__(self):
        return self.var.get().weight
    
    def insert(self, index, value):
        self.var.set(list_insert(self.var.get(), index, value))
    
    def __str__(self):
        return "TList(%r)" % stm.atomically(lambda: list(self))
    
    def __iter__(self):
        return list_iter(self.var.get())
    
    __repr__ = __str__


