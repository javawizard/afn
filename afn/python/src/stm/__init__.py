"""
A pure-Python software transactional memory system.

Have a look at the documentation for atomically() for more information.
"""

from threading import local as _Local, Lock as _Lock, Thread as _Thread
from Queue import Queue, Full
import weakref as weakref_module

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


class _BaseTransaction(_Transaction):
    """
    A toplevel transaction. This class takes care of committing values to the
    actual variables' values (and synchronizing on the global lock while doing
    so), blocking until vars are modified when a _RetryLater is caught, and so
    forth.
    """
    def __init__(self):
        _Transaction.__init__(self)
        self.parent = None
        self.check_values = set()
        self.retry_values = set()
        self.created_weakrefs = set()
        self.live_weakrefs = set()
        # Store off the transaction id we're starting at, so that we know if
        # things have changed since we started.
        with _global_lock:
            self.start = _last_transaction
    
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
    alive. Note that, for various reasons which I hope to outline here at some
    point, the callback function may or may not be called if the transaction
    creating the TWeakRef is aborted. If a callback that only runs when the
    transaction is not aborted is needed, one can do something like:
    
        should_run = TVar(False)
        should_run.set(True)
        def callback():
            if should_run.get():
                actual_callback()
        ref = TWeakRef(some_value, callback)
    
    Which takes advantage of the fact that TVars created during an aborted
    transaction retain the value they were constructed with (which is
    well-defined behavior that will never change).
    """
    def __init__(self, value, callback=None):
        self._queues = set()
        self._mature = False
        self._ref = value
        self._queues = set()
        # NOTE: callback will only be called if this transaction commits
        # successfully
        self._callback = callback
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
    
    def _check_clean(self):
        """
        Raises _RetryImmediately if we're mature, our referant has been
        garbage collected, and we're in our base transaction's live_weakrefs
        list (which indicates that we previously read our referant as live
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
        transaction's retry and check lists) and replacing our referant with
        an actual weakref.ref wrapper around it. This is called right at the
        end of the transaction in which this TWeakRef was created (and
        therefore only if it commits successfully) to make it live.
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
    
    If a transaction detects state that it cannot yet continue with, it can
    call retry(). Conceptually, one could think of retry() as causing the
    transaction to immediately abort and restart from the top. However, the
    actual implementation causes the transaction to block until at least one of
    the variables it accessed has been modified; it then immediately restarts.
    
    One can use retry() to obtain blocking behavior by doing something like
    this:
    
        if resources_available.get() > 0:
            resources_available.set(resources_available.get() - 1)
        else:
            retry()
    
    This will block until there is at least one resource available, and then
    atomically decrement the number of resources remaining.
    
    Note that inconsistent state (i.e. state produced when a transaction has
    only run half-way to completion) will never be seen by any other
    transaction, so such situations do not need to be accounted for in retry()
    logic.
    
    atomically() fully supports nested transactions. If a nested transaction
    throws an exception, the changes it made are reverted, and the exception
    propagated out of the call to atomically().
    
    Two alternative transactions, both of which may retry(), can be combined
    together such that the first to succeed (i.e. not retry) is executed and
    the value it produces returned with or_else(). This can be used to obtain
    behavior similar to the Unix system call select(). For example, to read a
    value from either q1 or q2, whichever produces a value first, and block
    until either one has a value:
    
        value = or_else(q1.get, q2.get)
    
    or_else can also be used to make non-blocking variants of blocking
    functions. For example, given a queue with a blocking get() function, we
    can get the queue's value or, if it does not currently have a value
    available, return None with:
    
        or_else(queue.get, lambda: None)
    
    If all of the transactions passed to or_else retry, or_else itself retries.
    
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
        _stm_state.current = transaction
        # Then run the transaction. _BaseTransaction's implementation takes care
        # of catching _RetryLater and blocking until one of the vars we read is
        # modified, then converting it into a _RetryImmediately exception.
        try:
            return transaction.run(function)
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
        finally:
            # Before we go, restore the current transaction to whatever it was
            # before we started.
            _stm_state.current = transaction.parent


def retry():
    """
    Retries the current transaction. See the documentation for atomically()
    for more information.
    """
    raise _RetryLater


def or_else(*args):
    """
    Runs (and returns the value produced by) the first function passed to this
    function that does not retry, or retries if all of the passed-in functions
    retry (or if no arguments are passed in). See the documentation for
    atomically() for more information.
    
    Note that each function passed in is automatically run in its own nested
    transaction, so that the effects of those that end up retrying are reverted
    before the next function is run.
    """
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












































