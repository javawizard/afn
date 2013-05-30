"""
An AVL tree implementation internally used by TList, TDict, and TSet.

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

class LookupError(Exception):
    """
    An exception that's used as the default one to throw from the various
    functions that throw exceptions when they drop off the edge of the tree
    without having found a node to process.
    """


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

LEFT = "LEFT"
STOP = "STOP"
RIGHT = "RIGHT"

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
    Returns a node providing the same in-order traversal as the specified node
    while adhering to the rules of AVL.
    
    Note that this only balances the tree at the current level; balance must be
    called every level up as a tree is being built to preserve the rules of AVL
    across the entire tree.
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

# selectors are of the form selector(node, key) -> (direction, new_key)
# node is the node to look at, key is the key passed in.
# direction is LEFT, STOP, or RIGHT, new_key is the key to pass to the next
# recursive call to the selector.

def insert(node, selector, key, value, exception=LookupError):
    if node is empty:
        return Node(empty, value, empty)
    direction, new_key = selector(node, key)
    if direction is LEFT: # Insert into the left subtree
        return balance(Node(insert(node.left, selector, new_key, value, exception), node.value, node.right))
    elif direction is RIGHT: # Insert into the right subtree
        return balance(Node(node.left, node.value, insert(node.right, selector, new_key, value, exception)))
    else: # Insert here; insert as the rightmost child of our left subtree
        return balance(Node(insert(node.left, lambda n, k: RIGHT, None, value, exception), node.value, node.right))


def replace(node, selector, key, value, exception=LookupError):
    # We're just replacing a node in the tree, so we don't need to do any
    # balancing
    if node is empty: # Walked right off the end of the tree
        raise exception()
    direction, new_key = selector(node, key)
    if direction is LEFT:
        return Node(replace(node.left, selector, new_key, value, exception), node.value, node.right)
    elif direction is RIGHT:
        return Node(node.left, node.value, replace(node.right, selector, new_key, value, exception))
    else:
        return Node(node.left, value, node.right)    


def delete(node, selector, key, exception=LookupError):
    if node is empty:
        raise exception()
    direction, new_key = selector(node, key)
    if direction is LEFT:
        return balance(Node(delete(node.left, selector, new_key, exception), node.value, node.right))
    elif direction is RIGHT:
        return balance(Node(node.left, node.value, delete(node.right, selector, new_key, exception)))
    else: # We're supposed to delete this node
        if node.left is empty and node.right is empty:
            # No children; just return empty
            return empty
        elif node.left is empty:
            # Only a right child; return it
            return node.right
        elif node.right is empty:
            # Only a left child; return it
            return node.left
        else:
            # Both a left and a right child, slightly more tricky. What we do
            # is pop the next item in the list and replace ourselves with it.
            new_value, new_right = pop_leftmost(node.right)
            return balance(Node(node.left, new_value, new_right))


def get(node, selector, key, exception=LookupError):
    if node is empty:
        raise exception()
    direction, new_key = selector(node, key)
    if direction is LEFT:
        return get(node.left, selector, new_key, exception)
    elif direction is RIGHT:
        return get(node.right, selector, new_key, exception)
    else: # Found the node
        return node.value


def traverse(node):
    if node is empty:
        return
    for child in traverse(node.left):
        yield child
    yield node.value
    for child in traverse(node.right):
        yield child









