
from afn import ttftree
from collections import MutableSequence
import stm

class TList(MutableSequence):
    """
    A transactional list.
    
    Internally, transactional lists are maintained with a single TVar holding a
    reference to a copy-on-write functional 2-3 finger tree (see the
    afn.ttftree module) using afn.ttftree.MEASURE_ITEM_COUNT as its measure.
    They thus give rise to some rather good performance characteristics:
    
          Time complexity:   Operations that run using this time complexity:
        
        +------------------+--------------------------------------------------+
        | amortized O(1)   | Inserting, removing, or looking up an item at    |
        |                  | either end of the list, e.g.:                    |
        |                  |     self.append(some_value)                      |
        |                  |     self.pop()                                   |
        |                  |     self.insert(0, some_value)                   |
        |                  |     some_value = list[0]                         |
        |                  |     some_value = list[-1]                        |
        +------------------+--------------------------------------------------+
        | O(1)             | len(self)                                        |
        |                  | iter(self) (but note that calling the returned   |
        |                  | iterator's next() method is amortized O(1))      |
        +------------------+--------------------------------------------------|
        | O(log n)         | Inserting, removing, or looking up an item by an |
        |                  | arbitrary index, e.g.:                           |
        |                  |     insert(n, some_value)                        |
        |                  |     some_value = list[n]                         |
        |                  |     list[n] = some_value                         |
        |                  |     del list[n], etc.                            |
        +------------------+--------------------------------------------------|
        | O(log min(m, n)) | Concatenating two lists, e.g.:                   |
        | where m and n    |     list1 + list2                                |
        | are the sizes of |     list1.extend(list2)                          |
        | the two lists    |                                                  |
        | involved         |                                                  |
        +------------------+--------------------------------------------------+
        | O(log r) where r | Slicing a list, e.g.:                            |
        | is the size of   |     list[m:n]                                    |
        | the returned     |                                                  |
        | list             |                                                  |
        +------------------+--------------------------------------------------+
    
    One nice property of using a copy-on-write tree is iteration: the iterator
    returned from iter(tlist) is a snapshot of the list at that point in time.
    The list can therefore be safely modified during iteration without
    affecting the items produced by the iteration.
    
    All of TList's functions must be called within an STM transaction, with the
    exception of __str__/__repr__, which, for the sake of convenience,
    wrap themselves in a call to stm.atomically() internally.
    """
    # Note: the performance table above is actually somewhat pessimistic: all
    # of the operations it mentions under O(log n) actually run in amortized
    # O(log min(n, size - n)) time (which is faster than O(log n)), thus giving
    # rise to amortized O(1) time when used on values at either end of the list
    # without any special casing.
    def __init__(self, initial_values=[]):
        self.var = stm.TVar(ttftree.Empty(ttftree.MEASURE_ITEM_COUNT))
        if initial_values:
            # If initial_values is an instance of Tree, we automatically get
            # O(1) performance here with no effort on our part: extend() is
            # overridden to just concatenate our respective trees in such a
            # case, and concatenation is O(log min(m, n)), which becomes O(1)
            # when one of the trees (our initial empty tree) is empty.
            self.extend(initial_values)
    
    def __getitem__(self, index):
        if isinstance(index, slice):
            # Index is a slice.
            start, stop, step = index.indices(len(self))
            # We don't support slices with step != 1 at the moment (they're
            # complicated to implement and I haven't had time to work out
            # whether they can be done efficiently with 2-3 finger trees)
            if step != 1:
                raise Exception("Slicing a list with a step other than 1 is "
                                "not supported right now.")
            # Partition the tree around stop and then start to extract the
            # relevant items
            mid, right = self.var.get().partition(lambda v: v > stop)
            left, mid = mid.partition(lambda v: v > start)
            # Then we just return a list pointing to the partitioned tree.
            new_list = TList()
            new_list.var.set(mid)
            return new_list
        else:
            # Index is an actual index, so return the relevant item.
            try:
                left, right = self.var.get().partition(lambda v: v > index)
                return right.get_first()
            except ttftree.TreeIsEmpty:
                raise IndexError(index)
    
    def __setitem__(self, index, value):
        try:
            left, right = self.var.get().partition(lambda v: v > index)
            self.var.set(left.add_last(value).append(right.without_first()))
        except ttftree.TreeIsEmpty:
            raise IndexError(index)
    
    def __delitem__(self, index):
        try:
            left, right = self.var.get().partition(lambda v: v > index)
            self.var.set(left.append(right.without_first()))
        except ttftree.TreeIsEmpty:
            raise IndexError(index)
    
    def __len__(self):
        return self.var.get().annotation
    
    def insert(self, index, value):
        left, right = self.var.get().partition(lambda v: v > index)
        self.var.set(left.add_last(value).append(right))
    
    def extend(self, values):
        # Override for efficiency: if values is another TList, we can just
        # concatenate its tree onto ours, which gets us O(log n) performance
        # instead of the usual O(n) required by extend.
        # MutableSequence overrides __iadd__ to delegate to extend, so this
        # will also improve __iadd__'s performance.
        if isinstance(values, TList):
            self.var.set(self.var.get().append(values.var.get()))
        else:
            # Not another TList, so delegate to MutableSequence (which will
            # call our append() for every item in the sequence).
            MutableSequence.extend(self, values)
    
    def __add__(self, other):
        if not isinstance(other, TList):
            return NotImplemented
        new_list = TList()
        new_list.extend(self)
        new_list.extend(other)
        return new_list
    
    def __radd__(self, other):
        if not isinstance(other, TList):
            return NotImplemented
        new_list = TList()
        # This is __radd__, so add other first
        new_list.extend(other)
        new_list.extend(self)
        return new_list
    
    def __str__(self):
        return "TList(%r)" % stm.atomically(lambda: list(self))
    
    def __iter__(self):
        return ttftree.value_iterator(self.var.get())
    
    __repr__ = __str__


