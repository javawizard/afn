

class Node(object):
    def __init__(self, left, value, right):
        self.left = left
        self.value = value
        self.right = right


def rotateLeft(node):
    a = node.left
    b = node.right.left
    c = node.right.right
    return Node(Node(a, node.value, b), node.right.value, c)


def rotateRight(node):
    a = node.left.left
    b = node.left.right
    c = node.right
    return Node(a, node.left.value, Node(b, node.value, c))


