"""
A module that will hopefully replace stm/__init__.py soon. It has the same
public API but will be implemented in a much cleaner way.
"""

from weakref import WeakKeyDictionary as _WeakKeyDictionary
from threading import Lock as _Lock, local as _Local
from Queue import Queue as _Queue, Full as _Full
from contextlib import contextmanager as _contextmanager

class _RetryImmediately(BaseException):
    def __str__(self):
        return ("This exception should never be caught. Make sure your code "
                "that uses the stm module only catches Exception instances, "
                "not BaseException instances as well.")

class _RetryLater(BaseException):
    def __str__(self):
        return ("This exception should never be caught. Make sure your code "
                "that uses the stm module only catches Exception instances, "
                "not BaseException instances as well.")


@_contextmanager
def _dummy():
    yield


class _State(_Local):
    def __init__(self):
        self._current = _master_transaction
    
    @property
    def current(self):
        if not self.current:
            raise Exception("No surrounding transaction")
    
    @_contextmanager
    def with_current(self, transaction):
        previous = self._current
        self._current = transaction
        yield
        self._current = previous


_state = _State()


class _VarState(object):
    # value: current value of the TVar
    # retry_queues: set of Queue.Queues that anything modifying self.value is
    # supposed to notify
    # modified: last version clock number of the transaction owning this
    # state in which a nested transaction modified us
    __slots__ = ["value", "retry_queues", "modified"]
    def __init__(self, value, modified):
        self.value = value
        self.modified = modified
        self.retry_queues = set()
    
    def notify_retry_queues(self):
        for q in self.retry_queues:
            try:
                q.put(None, False)
            except _Full:
                pass


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
        # Hang on, we should only ever acquire our own lock, and the parent
        # should do any of its own lock acquiring that it needs. We'll also
        # need to acquire our own lock while getting the value from a child...
        # So, the parent should have a method for getting the value of a
        # variable given a particular expected version, and the parent locks
        # on itself, then raises an exception if the variable's modified is
        # greater than the passed in version number, and if not it returns the
        # value. So in the parent, when we're asked for the variable's value
        # (probably with a version number of our own), we lock on our own lock
        # and go to do the same thing and see that we don't have a state for
        # it, so we call the parent passing in our own started_at as the
        # version number, then store it down as our value.
        # Note that we don't need to lock here on our own lock if we're
        # created in such a way that we know we'll never have any child
        # transactions run in parallel. I'll probably have a parameter for
        # that, and it'll save a bit of unlocking and locking.
        self.var_states = _WeakKeyDictionary()
        self.version_clock = 0
        self.lock = _Lock()
    
    def create_child(self):
        with self.lock:
            return _GenericTransaction(self, self.version_clock)
    
    def get_value(self, var, if_not_modified_since=None):
        with self.lock:
            state = self.var_states.get(var)
            if not state:
                value = self.parent.get_value(var, self.started_at)
                state = _VarState(value, self.version_clock)
                self.var_states[var] = state
            else:
                value = state.value
            if if_not_modified_since and state.modified > if_not_modified_since:
                raise _RetryImmediately
            return value
    
    def set_value(self, var, value, new_modified=None):
        state = self.var_states.get(var)
        if not state:
            state = _VarState(value, self.version_clock)
            self.var_states[var] = state
        else:
            state.value = value
            if new_modified:
                state = new_modified
                state.notify_retry_queues()
    
    def check_child_clean(self, transaction):
        # MUST BE CALLED WITH self.lock HELD.
        # Checks to see if the specified transaction could be applied cleanly
        # on top of this one, which should be the specified transaction's
        # parent, i.e. that nothing else has changed any of our variables that
        # the specified transaction wishes to change since it started running.
        # _RetryImmediately is thrown if it can't be cleanly applied.
        for var, state in transaction.var_states.iteritems():
            if self.var_states[var].modified > transaction.started_at:
                # Another transaction's modified our knowledge of this
                # var since this one started
                raise _RetryImmediately
    
    def commit_child(self, transaction):
        # Commit a child transaction's changes into this one.
        with self.lock:
            self.check_child_clean(transaction)
            self.version_clock += 1
            new_modified = self.version_clock
            for var, state in transaction.var_states.iteritems():
                self.set_value(var, state.value, new_modified)
    
    def block_child(self, transaction):
        # Block until the specified transaction could turn out differently
        # if it were re-run
        with self.lock:
            self.check_child_clean(transaction)
            q = _Queue(1)
            for var in transaction.var_states.iterkeys():
                self.var_states[var].retry_queues.append(q)
        q.get()
        with self.lock:
            for var in transaction.var_states.iterkeys():
                self.var_states[var].retry_queues.remove(q)
        raise _RetryImmediately
    
    def commit(self):
        self.parent.commit_child(self)
    
    def block(self):
        self.parent.block_child(self)


class _MasterTransaction(_GenericTransaction):
    # Subclass that does a few things:
    # Has a lock called self.lock
    # Locks on said lock in get_value
    # Doesn't allow variables to be modified directly, instead requires a
    # nested SerialTransaction (so changes can only be made with commit_child,
    # and values can only be read with get_value with a non-null
    # if_not_modified_since)
    # Implements proper blocking logic in block_child
    def __init__(self, *args):
        _GenericTransaction.__init__(*args)
        self.lock = _Lock()


class _UserTransaction(_GenericTransaction):
    # Subclass that does a few things:
    # Doesn't have a lock; requires all access to be serialized
    # Does allow variables to be directly accessed and modified
    # Doesn't implement blocking; block_child simply (re-)raises RetryLater


class TVar(object):
    __slots__ = ["__weakref__"]
    
    def __init__(self, value=None):
        _state.current.set_value(self, value)
    
    def get(self):
        return _state.current.get_value(self)


def atomically(function):
    while True:
        transaction = _state.current.create_child()
        try:
            with _state.with_current(transaction):
                result = function()
            transaction.commit()
            return result
        except _RetryImmediately:
            pass
        except _RetryLater:
            transaction.block()


_master_transaction = _GenericTransaction(None, None)

