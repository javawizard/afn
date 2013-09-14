
from afn import ttftree
from collections import MutableMapping
import stm


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
        _, length = self.var.get().annotation
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


















