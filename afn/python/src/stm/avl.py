"""
An AVL tree implementation internally used by TList and TDict.

This module provides an implementation of the AVL self-balancing binary tree
algorithm. TList and TDict use the trees provided by this module to store their
data.

At some point I hope to clean up the interface to this module a bit and then
make it a public module in its own right. For now, though, the interface is a
bit rough and some of the tree logic is in TList and TDict, but it's still
worth having a look around if you're curious about how AVL works.

Note that the version implemented here is copy-on-write. That is, once a node
is created, it is never modified; new nodes are created instead, pointing
(where appropriate) to old ones. The main reason for this is that TList and
TDict, by virtue of being transactional, need a copy-on-write data structure to
avoid having to inject TVars at every level of the tree, but interestingly
enough, a copy-on-write implementation of AVL turns out to be significantly
simpler to implement than an implementation that modifies the tree in place.

The core of the AVL algorithm is implemented in the balance() function. It
takes a node that may be out of balance by one level and returns a balanced
version of that node.
"""

from collections import MutableSequence

class _Empty(object):
    """
    Class of empty nodes. There is a singleton instance of this class, empty,
    that should be used; this class should never be instantiated separately.
    """
    def __init__(self):
        self.height = 0
        self.balance = 0
        self.weight = 0
    def __str__(self):
        return "empty"
    __repr__ = __str__
empty = _Empty()


class Node(object):
    """
    Class of nodes. Each node has a left child node, a value, and a right child
    node. Nodes also automatically compute their height (number of nodes to
    their farthest descendant, including themselves), balance (difference in
    height between the node's left and right subtrees), and weight (total
    number of nodes present in the subtree rooted at this node).
    """
    __slots__ = ["left", "value", "right", "height", "balance", "weight"]
    """
    Creates a new node with the specified left child (which may be either
    another Node instance or empty), value, and right child (which must obey
    the same constraints as the left child).
    """
    def __init__(self, left, value, right):
        self.left = left
        self.value = value
        self.right = right
        self.height = max(left.height, right.height) + 1
        # Positive numbers indicate left-heavy trees, negative numbers indicate
        # right-heavy trees
        self.balance = self.left.height - self.right.height
        self.weight = self.left.weight + 1 + self.right.weight
    
    def __str__(self):
        return "Node(%r, %r, %r)" % (self.left, self.value, self.right)
    
    __repr__ = __str__


def rotate_left(node):
    """
    Returns a node representing the left rotation of the specified node.
    """
    a = node.left
    b = node.right.left
    c = node.right.right
    return Node(Node(a, node.value, b), node.right.value, c)


def rotate_right(node):
    """
    Returns a node representing the right rotation of the specified node.
    """
    a = node.left.left
    b = node.left.right
    c = node.right
    return Node(a, node.left.value, Node(b, node.value, c))


def pop_leftmost(node):
    """
    Finds the leftmost descendant of the specified node (the first node that
    would be encountered, were a proper binary tree iteration to be performed
    on the specified node) and removes it, returning a tuple of the value of
    the node removed and the new node representing the tree with the leftmost
    node removed. If empty is passed in, an exception will be thrown.
    """
    if node.left is empty: # No left node, so this is the one we want. Return
        # its value, and replace it with its right node.
        return node.value, node.right
    else: # We can still keep walking left. Do so, but remember to balance on
        # our way back up.
        value, new_left = pop_leftmost(node.left)
        return value, balance(Node(new_left, node.value, node.right))


def balance(node):
    """
    Balances the specified node according to the rules of AVL.
    """
    # If the node is only off by one level, we just leave it be.
    if node.balance >= -1 and node.balance <= 1:
        return node
    if node.balance == -2:
        # The right child is two levels deeper than the left child, so we need 
        # to rotate left to get some of the weight moved to the left side. But 
        # first we see if the right child is left-heavy to avoid shifting too
        # much weight to the left side.
        if node.right.balance == 1:
            # Yep, it's left-heavy. Rotate it right to fix that.
            node = Node(node.left, node.value, rotate_right(node.right))
        # Then rotate ourselves left to shift the weight, and we're done.
        return rotate_left(node)
    elif node.balance == 2:
        # The left child is two levels deeper than the right child, so we
        # follow the same rules as for the opposite case, but with left and
        # right flipped.
        if node.left.balance == -1:
            node = Node(rotate_left(node.left), node.value, node.right)
        return rotate_right(node)
    else:
        # Tree's out of balance by more than two levels, which shouldn't ever
        # happen if we've implemented AVL correctly.
        raise Exception("Balance factor off (this is a bug in the AVL "
                        "tree implementation): %r for node %r" %
                        (node.balance, node))



