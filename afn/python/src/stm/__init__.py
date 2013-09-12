"""
A pure-Python software transactional memory system.

This module provides a software transactional memory system for Python. It
provides full support for isolated transactions, as well as the ability for
transactions to block as need be.

Have a look at the documentation for the atomically() function and the TVar
class. Those form the core building blocks of the STM system.
"""

from threading import local as _Local, Lock as _Lock, Thread as _Thread
from Queue import Queue, Full
import weakref as weakref_module
from contextlib import contextmanager

__all__ = ["TVar", "TWeakRef", "atomically", "retry", "or_else"]


class _RetryImmediately(BaseException):
    """
    Raised when a transaction needs to retry immediately. This happens when an
    attempt is made to read a variable that has been modified since the
    transaction started. It also happens just after the transaction has
    finished blocking in response to a _RetryLater.
    """
    pass

class _RetryLater(BaseException):
    """
    Raised when a transaction should retry at some later point, when at least
    one of the variables it accessed has been modified. This happens when
    retry() is called, and causes the toplevel transaction to block until one
    of the variables accessed in this transaction has been modified; the
    toplevel transaction then converts this into a _RetryImmediately.
    """
    pass


class _State(_Local):
    """
    A thread local holding the thread's current transaction
    """
    def __init__(self):
        self.current = None
    
    def get_current(self):
        """
        Returns the current transaction, or raises an exception if there is no
        current transaction.
        """
        if not self.current:
            raise Exception("No current transaction. The function you're "
                            "calling most likely needs to be wrapped in a "
                            "call to stm.atomically().")
        return self.current
    
    def get_base(self):
        return self.get_current().get_base_transaction()
    
    @contextmanager
    def with_current(self, transaction):
        old = self.current
        self.current = transaction
        try:
            yield
        finally:
            self.current = old

_stm_state = _State()
# Lock that we lock on while committing transactions
_global_lock = _Lock()
# Number of the last transaction to successfully commit. The first transaction
# run will change this to the number 1, the second transaction to the number
# 2, and so on.
_last_transaction = 0


class _Transaction(object):
    """
    An abstract class for transactions that provides functionality to track the
    values of variables read and written during this transaction. The subclass
    is responsible for providing the values of variables which are not yet
    known and for running functions within the scope of this transaction.
    """
    def __init__(self):
        self.vars = {}
    
    def get_real_value(self, var):
        """
        Returns the real value of the specified variable, possibly throwing
        _RetryImmediately if the variable has been modified since this
        transaction started. This will only be called once for any given var in
        any given transaction; the value will thereafter be stored in
        self.vars. This must be overridden by the subclass.
        """
        raise NotImplementedError
    
    def run(self, function):
        """
        Runs the specified function in the context of this transaction,
        committing changes as needed once finished. This must be overridden by
        the subclass.
        """
        raise NotImplementedError
    
    def get_value(self, var):
        """
        Looks up the value of the specified variable in self.vars and returns
        it, or calls self.get_real_value(var) (and then stores it in self.vars)
        if the specified variable is not in self.vars. This is a concrete
        function; subclasses need not override it.
        """
        try:
            return self.vars[var]
        except KeyError:
            value = self.get_real_value(var)
            self.vars[var] = value
            return value
    
    def set_value(self, var, value):
        """
        Sets the entry in self.vars for the specified variable to the specified
        value.
        """
        self.vars[var] = value
    
    def make_previously(self):
        """
        Returns a new transaction reflecting the state of this transaction
        just before it started.
        
        BaseTransaction will return another BaseTransaction with the same
        start attribute, which will cause it to throw RetryImmediately if
        anything's changed. NestedTransaction will most likely just return
        another NestedTransaction with the same parent.
        
        (Note that this new transaction can only be used until self is
        committed, and the new transaction should not itself be committed.)
        
        This is used mainly for implementing the previously() function; it's
        implemented by calling this function to obtain a previous transaction,
        then running the function passed to it in the context of that
        transaction and then aborting the transaction.
        """
        raise NotImplementedError


class _BaseTransaction(_Transaction):
    """
    A toplevel transaction. This class takes care of committing values to the
    actual variables' values (and synchronizing on the global lock while doing
    so), blocking until vars are modified when a _RetryLater is caught, and so
    forth.
    """
    def __init__(self, start=None):
        _Transaction.__init__(self)
        self.parent = None
        self.check_values = set()
        self.retry_values = set()
        self.created_weakrefs = set()
        self.live_weakrefs = set()
        # Store off the transaction id we're starting at, so that we know if
        # things have changed since we started.
        if not start:
            with _global_lock:
                start = _last_transaction
        self.start = start
    
    def get_base_transaction(self):
        return self
    
    def get_real_value(self, var):
        # Just check to make sure the variable hasn't been modified since we
        # started (and raise _RetryImmediately if it has), then return its real
        # value.
        with _global_lock:
            var._check_clean()
            self.check_values.add(var)
            self.retry_values.add(var)
            return var._real_value
    
    def run(self, function):
        global _last_transaction
        try:
            # First we actually run the transaction.
            result = function()
            # _Transaction appears to have run successfully, so commit it.
            self.commit()
            # And we're done!
            return result
        except _RetryLater:
            self.retry_block()
    
    def commit(self):
        global _last_transaction
        with _global_lock:
            # First, we need to make
            # sure nothing it used changed in the mean time.
            for item in self.check_values:
                item._check_clean()
            # Nothing changed, so we're good to commit. First we make
            # ourselves a new id.
            _last_transaction += 1
            modified = _last_transaction
            # Then we update the real values of all of the TVars. Note that
            # TVar._update_real_value takes care of notifying the TVar's
            # queues for us.
            for var, value in self.vars.iteritems():
                var._update_real_value(value, modified)
            # And then we tell all TWeakRefs created during this
            # transaction to mature
            for ref in self.created_weakrefs:
                ref._make_mature()
    
    def retry_block(self):
        # Received a retry request that made it all the way up to the top.
        # First, check to see if any of the variables we've accessed have
        # been modified since we started, which could change whether or not
        # we need to retry.
        with _global_lock:
            for item in self.check_values:
                item._check_clean()
            # Nope, none of them have changed. So now we create a queue,
            # then add it to all of the vars we need to watch.
            q = Queue(1)
            for item in self.retry_values:
                item._add_retry_queue(q)
        # Then we wait.
        q.get()
        # One of the vars was modified. Now we go remove ourselves from
        # the vars' queues.
        with _global_lock:
            for item in self.retry_values:
                item._remove_retry_queue(q)
        # And then we retry immediately.
        raise _RetryImmediately
    
    def make_previously(self):
        return _BaseTransaction(self.start)


class _NestedTransaction(_Transaction):
    """
    A nested transaction. This just wraps another transaction and persists
    changes to it upon committing unless the function to run throws an
    exception (of any sort, including _RetryImmediately and _RetryLater).
    """
    def __init__(self, parent):
        _Transaction.__init__(self)
        self.parent = parent
    
    def get_base_transaction(self):
        return self.parent.get_base_transaction()
    
    def get_real_value(self, var):
        # Just get the value from our parent.
        return self.parent.get_value(var)
    
    def run(self, function):
        # Run the function, then (if it didn't throw any exceptions;
        # _RetryImmediately, _RetryLater, or otherwise) copy our values into
        # our parent.
        result = function()
        self.commit()
        return result
    
    def commit(self):
        for var, value in self.vars.iteritems():
            self.parent.set_value(var, value)
    
    def make_previously(self):
        return _NestedTransaction(self.parent)


class TVar(object):
    """
    A transactional variable.
    
    TVars are the main primitives used within the STM system. They can only be
    read or written from within a call to atomically().
    
    More complex datatypes (such as TList, TDict, and TObject) are available in
    the tlist, tdict, and tobject modules, respectively.
    """
    __slots__ = ["_queues", "_real_value", "_modified", "__weakref__"]
    
    def __init__(self, value=None):
        """
        Creates a TVar with the specified initial value.
        """
        self._queues = set()
        self._real_value = value
        self._modified = 0
    
    def get(self):
        """
        Returns the current value of this TVar.
        
        This can only be called from within a call to atomically(). An
        exception will be thrown if this method is called elsewhere.
        """
        # Ask the current transaction for our value.
        return _stm_state.get_current().get_value(self)
    
    def set(self, value):
        """
        Sets the value of this TVar to the specified value.
        
        This can only be called from within a call to atomically(). An
        exception will be thrown if this method is called elsewhere.
        """
        # Set the specified value into the current transaction.
        _stm_state.get_current().set_value(self, value)
    
    value = property(get, set)
    
    def _check_clean(self):
        # Check to see if our underlying value has been modified since the
        # start of this transaction, which should be a BaseTransaction
        if self._modified > _stm_state.get_base().start:
            raise _RetryImmediately
    
    def _add_retry_queue(self, q):
        self._queues.add(q)
    
    def _remove_retry_queue(self, q):
        self._queues.remove(q)
    
    def _update_real_value(self, value, modified):
        # NOTE: This is always called while the global lock is acquired
        # Update our real value and modified transaction
        self._real_value = value
        self._modified = modified
        # Then notify all of the queues registered to us.
        for q in self._queues:
            try:
                q.put(None, False)
            except Full:
                pass 


class TWeakRef(object):
    """
    A transactional weak reference with a simple guarantee: the state of a
    given weak reference (i.e. whether or not it's been garbage collected yet)
    remains the same over the course of a given transaction. More specifically,
    if a TWeakRef's referent is garbage collected in the middle of a
    transaction that previously read the reference as alive, the transaction
    will be immediately aborted and restared from the top.
    
    A callback function can be specified when creating a TWeakRef; this
    function will be called in its own transaction when the value referred to
    by the TWeakRef is garbage collected, if the TWeakRef itself is still
    alive. Note that the callback function will only be called if the
    transaction in which this TWeakRef is created commits successfully.
    
    TWeakRefs fully support the retry() function; that is, a function such as
    the following works as expected, and blocks until the TWeakRef's referent
    is garbage collected:
    
    def block_until_garbage_collected(some_weak_ref):
        if some_weak_ref.get():
            retry()
    """
    def __init__(self, value, callback=None):
        self._queues = set()
        self._mature = False
        self._ref = value
        self._queues = set()
        # Use the TVar hack we previously mentioned in the docstring for
        # ensuring that the callback is only run if we commit. TODO: Double
        # check to make sure this is even necessary, as now that I think about
        # it we only create the underlying weakref when we commit, so we might
        # already be good to go.
        callback_check = TVar(False)
        callback_check.set(True)
        def actual_callback():
            if callback_check.get():
                callback()
        self._callback = actual_callback
        _stm_state.get_base().created_weakrefs.add(self)
    
    def get(self):
        """
        Returns the value that this weak reference refers to, or None if its
        value has been garbage collected.
        
        This will always return the same value over the course of a given
        transaction.
        """
        if self._mature:
            value = self._ref()
            if value is None and self in _stm_state.get_base().live_weakrefs:
                # Ref was live at some point during the past transaction but
                # isn't anymore
                raise _RetryImmediately
            # Value isn't inconsistent. Add it to the retry list (so that we'll
            # retry if we get garbage collected) and the check list (so that
            # we'll be checked for consistency again at the end of the
            # transaction).
            _stm_state.get_base().check_values.add(self)
            _stm_state.get_base().retry_values.add(self)
            # Then, if we're live, add ourselves to the live list, so that if
            # we later die in the transaction, we'll properly detect an
            # inconsistency
            if value is not None:
                _stm_state.get_base().live_weakrefs.add(self)
            # Then return our value.
            return value
        else:
            # We were just created during this transaction, so we haven't
            # matured (and had our ref wrapped in an actual weak reference), so
            # return our value.
            return self._ref
    
    def __call__(self):
        """
        An alias for self.get() provided for API compatibility with Python's
        weakref.ref class.
        """
        return self.get()
    
    def _check_clean(self):
        """
        Raises _RetryImmediately if we're mature, our referent has been
        garbage collected, and we're in our base transaction's live_weakrefs
        list (which indicates that we previously read our referent as live
        during this transaction).
        """
        if self._mature and self._ref() is None and self in _stm_state.get_base().live_weakrefs:
            # Ref was live during the transaction but has since been
            # dereferenced
            raise _RetryImmediately
    
    def _make_mature(self):
        """
        Matures this weak reference, setting self._mature to True (which causes
        all future calls to self.get to add ourselves to the relevant
        transaction's retry and check lists) and replacing our referent with
        an actual weakref.ref wrapper around it. This is called right at the
        end of the transaction in which this TWeakRef was created (and
        therefore only if it commits successfully) to make it live.
        
        The reason we keep around a strong reference until the end of the
        transaction in which the TWeakRef was created is to prevent a TWeakRef
        created in a transaction from being collected mid-way through the
        transaction and causing an immediate retry as a result, which would
        result in an infinite loop.
        """
        self._mature = True
        self._ref = weakref_module.ref(self._ref, self._on_value_dead)
    
    def _on_value_dead(self, ref):
        """
        Function passed to the underlying weakref.ref object to be called when
        it's collected. It spawns a thread (to avoid locking up whatever thread
        garbage collection is happening on) that notifies all of this
        TWeakRef's retry queues and then runs self._callback in a transaction.
        """
        def run():
            with _global_lock:
                for q in self._queues:
                    try:
                        q.put(None, False)
                    except Full:
                        pass
            if self._callback is not None:
                atomically(self._callback)
        _Thread(name="%r dead value notifier" % self, target=run).start()
    
    def _add_retry_queue(self, q):
        self._queues.add(q)
    
    def _remove_retry_queue(self, q):
        self._queues.remove(q)


def atomically(function):
    """
    Run the specified function in an STM transaction.
    
    Changes made to TVars from within a transaction will not be visible to
    other transactions until the transaction commits, and changes from other
    transactions started after this one started will not be seen by this one.
    The net effect is one of wrapping every transaction with a global lock, but
    without the loss of parallelism that would result.
    
    If the specified function throws an exception, the exception will be
    propagated out, and all of the changes made to TVars during the course of
    the transaction will be reverted.
    
    atomically() fully supports nested transactions. If a nested transaction
    throws an exception, the changes it made are reverted, and the exception
    propagated out of the call to atomically().
        
    The return value of atomically() is the return value of the function that
    was passed to it.
    """
    while True:
        # If we have no current transaction, create a _BaseTransaction.
        # Otherwise, create a _NestedTransaction with the current one as its
        # parent.
        if _stm_state.current:
            transaction = _NestedTransaction(_stm_state.current)
        else:
            transaction = _BaseTransaction()
        # Then set it as the current transaction
        with _stm_state.with_current(transaction):
            # Then run the transaction. _BaseTransaction's implementation takes care
            # of catching _RetryLater and blocking until one of the vars we read is
            # modified, then converting it into a _RetryImmediately exception.
            try:
                return transaction.run(function)
            # Note that we'll only get _RetryLater thrown here if we're in a
            # nested transaction, in which case we want it to propagate out, so
            # we don't catch it here.
            except _RetryImmediately:
                # We were asked to retry immediately. If we're a _BaseTransaction,
                # just continue. If we're a _NestedTransaction, propagate the
                # exception up. TODO: Figure out a way to move this logic into
                # individual methods on _Transaction that _BaseTransaction and
                # _NestedTransaction can override accordingly.
                if isinstance(transaction, _BaseTransaction):
                    continue
                else:
                    raise


def retry():
    """
    Provides support for transactions that block.
    
    This function, when called, indicates to the STM system that the caller has
    detected state with which it isn't yet ready to continue (for example, a
    queue from which an item is to be read is actually empty). The current
    transaction will be immediately aborted and automatically restarted once
    the STM system detects that it would produce a different result.
    
    This can be used to make, for example, a blocking queue from a list with a
    function like the following:
    
    def pop_or_block(some_list):
        if len(some_list) > 0:
            return some_list.pop()
        else:
            retry()
    
    Functions making use of retry() can be multiplexed, a la Unix's select
    system call, with the or_else function. See its documentation for more
    information.
    """
    # Make sure we're in a transaction
    _stm_state.get_current()
    raise _RetryLater


def or_else(*args):
    """
    Runs (and returns the value produced by) the first function passed to this
    function that does not retry (see the documentation of the retry()
    function), or retries if all of the passed-in functions
    retry (or if no arguments are passed in). See the documentation for
    retry() for more information. 
    
    This function could be considered the STM equivalent of Unix's select()
    system call. One could, for example, read an item from the first of two
    queues, q1 and q2, to actually produce an item with something like this:
    
    item = or_else(q1.get, q2.get)
    
    or_else can also be used to make non-blocking variants of blocking
    functions. For example, given one of our queues above, we can get the first
    value available from the queue or, if it does not currently have any values
    available, return None with:
    
    item = or_else(q1.get, lambda: None)
    
    Note that each function passed in is automatically run in its own nested
    transaction, so that the effects of those that end up retrying are reverted
    and only the effects of the function that succeeds are persisted.
    """
    # Make sure we're in a transaction
    _stm_state.get_current()
    for function in args:
        # Try to run each function in sequence, in its own transaction so that
        # if it raises _RetryLater (or any other exception) its effects will be
        # undone.
        try:
            return atomically(function)
        except _RetryLater:
            # Requested a retry, so move on to the next alternative
            pass
    # All of the alternatives retried, so retry ourselves.
    retry()


def previously(function, toplevel=False):
    """
    (This function is experimental and will likely change in the future. I'd
    also like feedback on how useful it is.)
    
    Returns the value that the specified function would have returned had it
    been run in a transaction just before the current transaction started.
    
    If toplevel is False, the specified function will be run as if it were just
    before the start of the innermost nested transaction, if any. If toplevel
    is True, the specified function will be run as if it were just before the
    start of the outermost transaction.
    """
    # We don't need any special retry handling in _BaseTransaction like I
    # thought we would because we're calling the function directly, not calling
    # transaction.run(function), so we'll get _RetryImmediately and _RetryLater
    # passed back out to us.
    if toplevel:
        current = _stm_state.get_base()
    else:
        current = _stm_state.get_current()
    transaction = current.make_previously()
    try:
        with _stm_state.with_current(transaction):
            return function()
    finally:
        if isinstance(transaction, _BaseTransaction):
            current.check_values.update(transaction.check_values)
            current.retry_values.update(transaction.retry_values)
        # If it's a nested transaction, it will have already modified our base
        # by virtue of using our base as its parent, so we don't need to do
        # anything else.












































