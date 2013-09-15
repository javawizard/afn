"""
Some useful data types built on top of the STM system.

This module provides several transactional data types built on top of the
primitives provided by the STM module.
"""

from afn import ttftree
from collections import MutableSequence, MutableMapping, MutableSet
import stm
from stm.timeout import Timeout


class Empty(Exception):
    """
    Exception thrown from BroadcastEndpoint.get() when block=False is passed
    in and no items are currently available.
    """
    pass


class Full(Exception):
    pass


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
    # without any special casing needed.
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


class TDict(MutableMapping):
    """
    UPDATE: This now uses 2-3 finger trees. Update accordingly.
    
    A transactional dictionary.
    
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
    def __init__(self, initial_values=None):
        self.var = stm.TVar(ttftree.Empty(ttftree.CompoundMeasure(ttftree.MeasureItemCount(), ttftree.TranslateMeasure(lambda (k, v): k, ttftree.MeasureLastItem()))))
        if initial_values:
            # Optimize to O(1) if we're cloning another TDict
            if isinstance(initial_values, TDict):
                self.var.set(initial_values.var.get())
            # Initializing from another dict-like object
            elif hasattr(initial_values, "keys"):
                for k in initial_values.keys():
                    self[k] = initial_values[k]
            # Initializing from a sequence of 2-tuples
            else:
                for k, v in initial_values:
                    self[k] = v
    
    def __getitem__(self, key):
        left, right = self.var.get().partition(lambda (i, k): k >= key)
        if right.is_empty or right.get_first()[0] != key:
            raise KeyError(key)
        return right.get_first()[1]
    
    def __setitem__(self, key, value):
        left, right = self.var.get().partition(lambda (i, k): k >= key)
        if not right.is_empty and right.get_first()[0] == key:
            right = right.without_first()
        self.var.set(left.add_last((key, value)).append(right))
    
    def __delitem__(self, key):
        left, right = self.var.get().partition(lambda (i, k): k >= key)
        if right.is_empty or right.get_first()[0] != key:
            raise KeyError(key)
        self.var.set(left.append(right.without_first()))
    
    def __iter__(self):
        for k, _ in ttftree.value_iterator(self.var.get()):
            yield k
    
    def __len__(self):
        length, _ = self.var.get().annotation
        return length
    
    def iterkeys(self):
        return iter(self)
    
    def keys(self):
        return list(self.iterkeys())
    
    def itervalues(self):
        for _, v in ttftree.value_iterator(self.var.get()):
            yield v
    
    def values(self):
        return list(self.itervalues())
    
    def iteritems(self):
        return ttftree.value_iterator(self.var.get())
    
    def items(self):
        return list(self.iteritems())
    
    def __str__(self):
        return "TDict(%r)" % stm.atomically(lambda: dict(self))
    
    __repr__ = __str__


class TObject(object):
    """
    An abstract class that causes all of its subclass's attributes to be backed
    by a TDict. This results in the subclass's attributes being transactional,
    so that they can be modified during a transaction without having to
    explicitly wrap all of them with TVars.
    """
    def __init__(self):
        object.__setattr__(self, "_tobject_dict", TDict())
    
    def __getattr__(self, name):
        if name == "_tobject_dict":
            return object.__getattribute__(self, name)
        return self._tobject_dict[name]
    
    def __setattr__(self, name, value):
        self._tobject_dict[name] = value
    
    def __delattr__(self, name):
        del self._tobject_dict[name]
    
    def __dir__(self):
        return self._tobject_dict.keys()


class TSet(MutableSet):
    """
    A transactional mutable set.
    
    Right now, this actually just uses an underlying Python set which it copies
    on every write, so it's woefully inefficient. I'll be changing it to use
    the AVL trees that TList and TDict use soon, but I need to figure out how
    to properly implement weak sets in terms of them first. (I could always
    just have weak nodes pointing to dereferenced objects collected only during
    iteration, but then the iterators can't be completely isolated from the set
    itself, so that needs some thought.)
    """
    def __init__(self, initial_items=set(), backing_type=set):
        self.backing_type = backing_type
        self._var = stm.TVar(backing_type(initial_items))
    
    def __contains__(self, item):
        return self._var.get().__contains__(item)
    
    def __iter__(self):
        return self._var.get().__iter__()
    
    def __len__(self):
        return self._var.get().__len__()
    
    def add(self, item):
        new_set = self.backing_type(self._var.get())
        new_set.add(item)
        self._var.set(new_set)
    
    def discard(self, item):
        new_set = self.backing_type(self._var.get())
        new_set.discard(item)
        self._var.set(new_set)


class _BroadcastItem(object):
    """
    An item that has been added to the queue. Items themselves are immutable,
    but they contain a TVar pointing to the next item added to the queue, or
    None if a "next item" hasn't been added yet.
    """
    __slots__ = ["value", "next"]
    
    def __init__(self, value):
        self.value = value
        self.next = stm.TVar()


class BroadcastQueue(TObject):
    """
    A queue that can have multiple endpoints from which items can be consumed.
    
    Endpoints are created by calling new_endpoint(). Each endpoint initially
    starts out empty; items become available as soon as the creating queue's
    put() function is called.
    
    Items inserted into the queue with put() become available on all endpoints
    to consume. This allows BroadcastQueues to be used to broadcast values to
    several different consumers.
    
    When an endpoint is no longer needed, it can be simply discarded. Endpoints
    hold a reference to the queue they were created from, not the other way
    around, so they will be immediately garbage collected and any items unread
    by the endpoint but not by any other endpoint immediately reclaimed.
    
    An interesting side effect of this is that adding items to a queue that has
    never had any endpoints created from it, or one that has had all of its
    endpoints discarded, silently discards the items added to it, and is thus
    a no-op.
    """
    def __init__(self):
        TObject.__init__(self)
        self._var = stm.TVar(None)
    
    def put(self, value):
        """
        Inserts an item into this queue. The item will then become available on
        all endpoints created from it.
        """
        item = _BroadcastItem(value)
        self._var.set(item)
        self._var = item.next
    
    def new_endpoint(self):
        """
        Creates a new endpoint that receives values added to this queue. The
        endpoint initially starts out empty; items will appear on it as soon as
        put() is called next.
        """
        return BroadcastEndpoint(self._var)


class BroadcastEndpoint(TObject):
    """
    A broadcast endpoint from which items can be read.
    
    Normally, you won't need to create instances of this class yourself;
    they're typically obtained by calling new_endpoint on a BroadcastQueue
    instance.
    """
    def __init__(self, var):
        TObject.__init__(self)
        self._var = var
    
    def get(self, block=True, timeout=None):
        """
        Removes and returns the next available item from this endpoint.
        
        If block is False and there aren't any items currently available on
        this endpoint, Empty will be raised. If block is True, this function
        retries. If timeout is specified and there still aren't any items
        available on this endpoint after that many seconds, Timeout will be
        raised.
        """
        if self._var.get() is None:
            if block:
                stm.retry(resume_after=timeout)
                raise Timeout
            else:
                raise Empty
        else:
            item = self._var.get()
            self._var = item.next
            return item.value



