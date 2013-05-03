from collections import MutableSequence

class _Empty(object):
    def __init__(self):
        self.height = 0
        self.balance = 0
        self.weight = 0
    def __str__(self):
        return "empty"
    __repr__ = __str__
empty = _Empty()


class Node(object):
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
    a = node.left
    b = node.right.left
    c = node.right.right
    return Node(Node(a, node.value, b), node.right.value, c)


def rotate_right(node):
    a = node.left.left
    b = node.left.right
    c = node.right
    return Node(a, node.left.value, Node(b, node.value, c))


def pop_leftmost(node):
    if node.left is empty:
        return node.value, node.right
    else:
        value, new_left = pop_leftmost(node.left)
        return value, balance(new_left)


def balance(node):
    if node.balance >= -1 and node.balance <= 1:
        return node
    if node.balance == -2:
        right_balance = node.right.balance
        if right_balance == 1:
            node = Node(node.left, node.value, rotate_right(node.right))
        return rotate_left(node)
    elif node.balance == 2:
        left_balance = node.left.balance
        if left_balance == -1:
            node = Node(rotate_left(node.left), node.value, node.right)
        return rotate_right(node)



