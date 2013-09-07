"""
Experimental implementation of functional 2-3 finger trees that I'm working on.

I'm hoping to replace stm.avl with these, which'll give O(1) list insertion,
appending, and removal from either end to stm.tlist.TList.

I'm also planning on implementing custom measures (which could also be done
with stm.avl), which'll give the ability for, e.g., an ordered list to also
function as a priority queue.
"""

from collections import Sequence

class Node(Sequence):
    def __init__(self, *values):
        if len(values) not in (2, 3):
            raise Exception("Nodes must have 2 or 3 children")
        self._values = values
    
    def __getitem__(self, index):
        if isinstance(index, slice):
            return Node(*self._values[index])
        else:
            return self._values[index]
    
    def __len__(self):
        return len(self._values)
    
    def __add__(self, other):
        return Node(*self._values + other._values)
    
    def __repr__(self):
        return "Node(%s)" % ", ".join([repr(v) for v in self])


class Digit(Sequence):
    def __init__(self, *values):
        if len(values) not in (1, 2, 3, 4):
            raise Exception("Digits must have 1, 2, 3, or 4 children")
        self._values = values
    
    def __getitem__(self, index):
        if isinstance(index, slice):
            return Digit(*self._values[index])
        else:
            return self._values[index]
    
    def __len__(self):
        return len(self._values)
    
    def __add__(self, other):
        return Digit(*self._values + other._values)
    
    def __repr__(self):
        return "Digit(%s)" % ", ".join([repr(v) for v in self])


class Tree(object):
    # empty -> bool
    
    # get_first() -> item
    # remove_first() -> Tree
    # add_first(item) -> Tree
    # get_last() -> item
    # remove_last() -> Tree
    # add_last(item) -> Tree
    
    # prepend(tree) -> Tree
    # append(tree) -> Tree
    pass


class Empty(Tree):
    empty = True
    
    def get_first(self):
        raise ValueError
    
    def remove_first(self):
        raise ValueError
    
    def add_first(self, item):
        return Single(item)
    
    def get_last(self):
        raise ValueError
    
    def remove_last(self):
        raise ValueError
    
    def add_last(self, item):
        return Single(item)
    
    def __repr__(self):
        return "Empty()"


class Single(Tree):
    empty = False
    
    def __init__(self, item):
        self.item = item
    
    def get_first(self):
        return self.item
    
    def remove_first(self):
        return Empty()
    
    def add_first(self, new_item):
        return Deep(Digit(new_item), Empty(), Digit(self.item))
    
    def get_last(self):
        return self.item
    
    def remove_last(self):
        return Empty()
    
    def add_last(self, new_item):
        return Deep(Digit(self.item), Empty(), Digit(new_item))
    
    def __repr__(self):
        return "Single(%r)" % (self.item,)


class Deep(Tree):
    empty = False
    
    def __init__(self, left, spine, right):
        self.left = left
        self.spine = spine
        self.right = right
    
    def get_first(self):
        return self.left[0]
    
    def remove_first(self):
        if len(self.left) > 1:
            return Deep(self.left[1:], self.spine, self.right)
        elif not self.spine.empty:
            return Deep(Digit(*self.spine.get_first()), self.spine.remove_first(), self.right)
        elif len(self.right) == 1:
            return Single(self.right[0])
        elif len(self.right) == 2:
            return Deep(self.right[0:1], self.spine, self.right[1:2])
        elif len(self.right) == 3:
            return Deep(self.right[0:2], self.spine, self.right[2:3])
        elif len(self.right) == 4:
            return Deep(self.right[0:2], self.spine, self.right[2:4])
    
    def add_first(self, new_item):
        if len(self.left) < 4:
            return Deep(Digit(new_item) + self.left, self.spine, self.right)
        else:
            node = Node(self.left[1], self.left[2], self.left[3])
            return Deep(Digit(new_item, self.left[0]), self.spine.add_first(node), self.right)
    
    def get_last(self):
        return self.right[-1]
    
    def remove_last(self):
        if len(self.right) > 1:
            return Deep(self.left, self.spine, self.right[:-1])
        elif not self.spine.empty:
            return Deep(self.left, self.spine.remove_last(), Digit(*self.spine.get_last()))
        elif len(self.left) == 1:
            return Single(self.left[0])
        elif len(self.left) == 2:
            return Deep(self.left[0:1], self.spine, self.left[1:2])
        elif len(self.left) == 3:
            return Deep(self.left[0:1], self.spine, self.left[1:3])
        elif len(self.left) == 4:
            return Deep(self.left[0:2], self.spine, self.right[2:4])
    
    def add_last(self, new_item):
        if len(self.right) < 4:
            return Deep(self.left, self.spine, self.right + Digit(new_item))
        else:
            node = Node(self.right[0], self.right[1], self.right[2])
            return Deep(self.left, self.spine.add_last(node), Digit(self.right[3], new_item))
    
    def __repr__(self):
        return "Deep(left=%r, spine=%r, right=%r)" % (self.left, self.spine, self.right)
    





















