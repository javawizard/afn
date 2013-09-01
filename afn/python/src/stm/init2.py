"""
A module that will hopefully replace stm/__init__.py soon. It has the same
public API but will be implemented in a much cleaner way.
"""

from weakref import WeakKeyDictionary as _WeakKeyDictionary
from threading import Lock as _Lock
from Queue import Queue as _Queue

class _VarState(object):
    # value: current value of the TVar
    # retry_queues: set of Queue.Queues that anything modifying self.value is
    # supposed to notify
    # modified: last version clock number of the transaction owning this
    # state in which a nested transaction modified us
    __slots__ = ["value", "retry_queues", "modified"]

class _GenericTransaction(object):
    def __init__(self, parent, started_at):
        self.parent = parent
        self.started_at = started_at
        # VarState objects for any vars read or written within this
        # transaction. When a var is read and not here, its parent's lock is
        # acquired, and its parent's state retrieved, then if its parent's
        # state's modified is higher than our started_at, we retry
        # immediately. Otherwise we get the value from the parent's state,
        # unlock, then create our own state with the given value and a
        # modified of our current modified, and...
        self.var_states = _WeakKeyDictionary()
        self.version_clock = 0
        self.lock = _Lock()
    
    def commit(self):
        with self.parent.lock:
            for state 


class TVar(object):
    __slots__ = ["__weakref__"]
