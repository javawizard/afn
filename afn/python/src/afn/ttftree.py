"""
Experimental implementation of functional 2-3 finger trees that I'm working on.

I'm hoping to replace stm.avl with these, which'll give O(1) list insertion,
appending, and removal from either end to stm.tlist.TList.

I'm also planning on implementing custom measures (which could also be done
with stm.avl), which'll give the ability for, e.g., an ordered list to also
function as a priority queue.
"""

# Credit goes to http://maniagnosis.crsr.net/2010/11/finger-trees.html for
# giving my brain just the right information it needed to finally understand
# 2-3 finger trees 

# FIXME: At points where we create a new spine, we need its measure to be
# create_node_measure(self.measure) as it'll contain Nodes of whatever we
# contain

from collections import Sequence

class Measure(object):
    def __init__(self, convert, operator, identity):
        self.convert = convert
        self.operator = operator
        self.identity = identity


def create_node_measure(measure):
    """
    Given a particular measure, return an identical measure that operates on
    Node objects containing values accepted by the given measure.
    """
    return Measure(lambda node: node.annotation, measure.operator, measure.identity)


class Node(Sequence):
    def __init__(self, measure, *values):
        if len(values) not in (2, 3):
            raise Exception("Nodes must have 2 or 3 children")
        self._values = values
        self.measure = measure
        self.annotation = reduce(measure.operator, map(measure.convert, values))
    
    def __getitem__(self, index):
        if isinstance(index, slice):
            return Node(self.measure, *self._values[index])
        else:
            return self._values[index]
    
    def __len__(self):
        return len(self._values)
    
    def __add__(self, other):
        return Node(self.measure, *self._values + other._values)
    
    def __repr__(self):
        return "<Node: %s>" % ", ".join([repr(v) for v in self])


class Digit(Sequence):
    def __init__(self, measure, *values):
        if len(values) not in (1, 2, 3, 4):
            raise Exception("Digits must have 1, 2, 3, or 4 children; the "
                            "children given were %r" % list(values))
        self._values = values
        self.measure = measure
        self.annotation = reduce(measure.operator, map(measure.convert, values))
    
    def __getitem__(self, index):
        if isinstance(index, slice):
            return Digit(self.measure, *self._values[index])
        else:
            return self._values[index]
    
    def __len__(self):
        return len(self._values)
    
    def __add__(self, other):
        return Digit(self.measure, *self._values + other._values)
    
    def __repr__(self):
        return "<Digit: %s>" % ", ".join([repr(v) for v in self])


class Tree(object):
    # empty -> bool
    
    # get_first() -> item
    # remove_first() -> Tree
    # add_first(item) -> Tree
    # get_last() -> item
    # remove_last() -> Tree
    # add_last(item) -> Tree
    
    # append(tree) -> Tree
    # prepend(tree) -> Tree
    pass

class Empty(Tree):
    empty = True
    
    def __init__(self, measure):
        self.measure = measure
    
    def get_first(self):
        raise ValueError
    
    def remove_first(self):
        raise ValueError
    
    def add_first(self, item):
        return Single(self.measure, item)
    
    def get_last(self):
        raise ValueError
    
    def remove_last(self):
        raise ValueError
    
    def add_last(self, item):
        return Single(self.measure, item)
    
    def prepend(self, other):
        return other
    
    def append(self, other):
        return other
    
    def iterate_values(self):
        if False:
            yield None
    
    def __repr__(self):
        return "<Empty>"


class Single(Tree):
    empty = False
    
    def __init__(self, measure, item):
        self.measure = measure
        self.item = item
    
    def get_first(self):
        return self.item
    
    def remove_first(self):
        return Empty(self.measure)
    
    def add_first(self, new_item):
        return Deep(self.measure, Digit(self.measure, new_item), Empty(self.measure), Digit(self.measure, self.item))
    
    def get_last(self):
        return self.item
    
    def remove_last(self):
        return Empty(self.measure)
    
    def add_last(self, new_item):
        return Deep(self.measure, Digit(self.measure, self.item), Empty(self.measure), Digit(self.measure, new_item))
    
    def prepend(self, other):
        return other.add_last(self.item)
    
    def append(self, other):
        return other.add_first(self.item)
    
    def iterate_values(self):
        yield self.item
    
    def __repr__(self):
        return "<Single: %r>" % (self.item,)


class Deep(Tree):
    empty = False
    
    def __init__(self, measure, left, spine, right):
        self.measure = measure
        self.left = left
        self.spine = spine
        self.right = right
    
    def get_first(self):
        return self.left[0]
    
    def remove_first(self):
        if len(self.left) > 1:
            return Deep(self.measure, self.left[1:], self.spine, self.right)
        elif not self.spine.empty:
            return Deep(self.measure, Digit(self.measure, *self.spine.get_first()), self.spine.remove_first(), self.right)
        elif len(self.right) == 1:
            return Single(self.measure, self.right[0])
        elif len(self.right) == 2:
            return Deep(self.measure, self.right[0:1], self.spine, self.right[1:2])
        elif len(self.right) == 3:
            return Deep(self.measure, self.right[0:2], self.spine, self.right[2:3])
        elif len(self.right) == 4:
            return Deep(self.measure, self.right[0:2], self.spine, self.right[2:4])
    
    def add_first(self, new_item):
        if len(self.left) < 4:
            return Deep(self.measure, Digit(self.measure, new_item) + self.left, self.spine, self.right)
        else:
            node = Node(self.measure, self.left[1], self.left[2], self.left[3])
            return Deep(self.measure, Digit(self.measure, new_item, self.left[0]), self.spine.add_first(node), self.right)
    
    def get_last(self):
        return self.right[-1]
    
    def remove_last(self):
        if len(self.right) > 1:
            return Deep(self.measure, self.left, self.spine, self.right[:-1])
        elif not self.spine.empty:
            return Deep(self.measure, self.left, self.spine.remove_last(), Digit(self.measure, *self.spine.get_last()))
        elif len(self.left) == 1:
            return Single(self.measure, self.left[0])
        elif len(self.left) == 2:
            return Deep(self.measure, self.left[0:1], self.spine, self.left[1:2])
        elif len(self.left) == 3:
            return Deep(self.measure, self.left[0:1], self.spine, self.left[1:3])
        elif len(self.left) == 4:
            return Deep(self.measure, self.left[0:2], self.spine, self.left[2:4])
    
    def add_last(self, new_item):
        if len(self.right) < 4:
            return Deep(self.measure, self.left, self.spine, self.right + Digit(self.measure, new_item))
        else:
            node = Node(self.measure, self.right[0], self.right[1], self.right[2])
            return Deep(self.measure, self.left, self.spine.add_last(node), Digit(self.measure, self.right[3], new_item))
    
    def prepend(self, other):
        return other.append(self)
    
    def append(self, other):
        if not isinstance(other, Deep):
            return other.prepend(self)
        return Deep(self.measure, self.left, self._fold_up(self, other), other.right)
    
    @staticmethod
    def _fold_up(left_tree, right_tree):
        middle_items = list(left_tree.right) + list(right_tree.left)
        spine = left_tree.spine
        while middle_items:
            # Could be optimized to not remove items from the front of a list,
            # which is a bit slow; perhaps reverse middle_items and pop from
            # the end of the list, or use a sliding index that we increment as
            # we go and don't modify the list at all
            if len(middle_items) == 2:
                spine = spine.add_last(Node(self.measure, middle_items[0], middle_items[1]))
                del middle_items[0:2]
            elif len(middle_items) == 4:
                spine = spine.add_last(Node(self.measure, middle_items[0], middle_items[1]))
                spine = spine.add_last(Node(self.measure, middle_items[2], middle_items[3]))
                del middle_items[0:4]
            else:
                spine = spine.add_last(Node(self.measure, middle_items[0], middle_items[1], middle_items[2]))
                del middle_items[0:3]
        return spine.append(right_tree.spine)
    
    def iterate_values(self):
        for v in self.left:
            yield v
        for node in self.spine.iterate_values():
            for v in node:
                yield v
        for v in self.right:
            yield v
    
    def __repr__(self):
        return "<Deep: left=%r, spine=%r, right=%r>" % (self.left, self.spine, self.right)
    





















