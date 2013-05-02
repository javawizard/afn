
from threading import local as Local, Lock
from Queue import Queue, Full

class RetryImmediately(BaseException):
    pass

class RetryLater(BaseException):
    pass


class State(Local):
    def __init__(self):
        self.current = None
    
    def get_current(self):
        if not self.current:
            raise Exception("No current transaction")
        return self.current

stm_state = State()
global_lock = Lock()
last_transaction = 0


class Transaction(object):
    def __init__(self):
        self.vars = {}
    
    def get_real_value(self, var):
        raise NotImplementedError
    
    def run(self, function):
        raise NotImplementedError
    
    def get_value(self, var):
        try:
            return self.vars[var]
        except KeyError:
            value = self.get_real_value(var)
            self.vars[var] = value
            return value
    
    def set_value(self, var, value):
        self.vars[var] = value
    
    def try_commit(self):
        raise NotImplementedError


class BaseTransaction(Transaction):
    def __init__(self):
        Transaction.__init__(self)
        self.parent = None
        # Store off the transaction id we're starting at, so that we know if
        # things have changed since we started.
        with global_lock:
            self.start = last_transaction
    
    def get_real_value(self, var):
        # Just check to make sure the variable hasn't been modified since we
        # started (and raise RetryImmediately if it has), then return its real
        # value.
        with global_lock:
            if var.modified > self.start:
                raise RetryImmediately
            return var.real_value
    
    def run(self, function):
        global last_transaction
        try:
            # First we actually run the transaction.
            result = function()
            # Transaction appears to have run successfully. Now we need to make
            # sure nothing it used changed in the mean time.
            with global_lock:
                for var in self.vars:
                    if var.modified > self.start:
                        # Var was modified since we started, so we need to
                        # retry against its new value.
                        raise RetryImmediately
                # Nothing changed, so we're good to commit. First we make
                # ourselves a new id.
                last_transaction += 1
                modified = last_transaction
                # Then we update the real values of all of the TVars. Note that
                # TVar.update_real_value takes care of notifying the TVar's
                # queues for us.
                for var, value in self.vars.iteritems():
                    var.update_real_value(value, modified)
                # And we're done!
                return result
        except RetryLater:
            # Received a retry request that made it all the way up to the top.
            # First, check to see if any of the variables we've accessed have
            # been modified since we started, which could change whether or not
            # we need to retry.
            with global_lock:
                for var in self.vars:
                    if var.modified > self.start:
                        # Yep, one changed. Retry immediately.
                        raise RetryImmediately
                # Nope, none of them have changed. So now we create a queue,
                # then add it to all of the vars we need to watch.
                q = Queue(1)
                for var in self.vars:
                    var.queues.add(q)
            # Then we wait.
            q.get()
            # One of the vars was modified. Now we go remove ourselves from
            # the vars' queues.
            with global_lock:
                for var in self.vars:
                    var.queues.remove(q)
            # And then we retry immediately.
            raise RetryImmediately


class NestedTransaction(Transaction):
    def __init__(self, parent):
        Transaction.__init__(self)
        self.parent = parent
    
    def get_real_value(self, var):
        # Just get the value from our parent.
        return self.parent.get_value(var)
    
    def run(self, function):
        # Run the function, then (if it didn't throw any exceptions;
        # RetryImmediately, RetryLater, or otherwise) copy our values into
        # our parent.
        result = function()
        for var, value in self.vars.iteritems():
            self.parent.set_value(var, value)
        return result


class TVar(object):
    def __init__(self, value=None):
        self.queues = set()
        self._real_value = value
        self.modified = 0
    
    def get(self):
        # Ask the current transaction for our value.
        return stm_state.get_current().get_value(self)
    
    def set(self, value):
        # Set the specified value into the current transaction.
        stm_state.get_current().set_value(self, value)
    
    value = property(get, set)
    
    def update_real_value(self, value, modified):
        # Update our real value and modified transaction
        self._real_value = value
        self.modified = modified
        # Then notify all of the queues registered to us.
        for q in self.queues:
            try:
                q.put(None, False)
            except Full:
                pass 
    
    # We do this mainly to prevent real_value from being accidentally directly
    # written.
    @property
    def real_value(self):
        return self._real_value


def atomically(function):
    while True:
        # If we have no current transaction, create a BaseTransaction.
        # Otherwise, create a NestedTransaction with the current one as its
        # parent.
        if stm_state.current:
            transaction = NestedTransaction(stm_state.current)
        else:
            transaction = BaseTransaction()
        # Then set it as the current transaction
        stm_state.current = transaction
        # Then run the transaction. BaseTransaction's implementation takes care
        # of catching RetryLater and blocking until one of the vars we read is
        # modified, then converting it into a RetryImmediately exception.
        try:
            return transaction.run(function)
        except RetryImmediately:
            # We were asked to retry immediately. If we're a BaseTransaction,
            # just continue. If we're a NestedTransaction, propagate the
            # exception up. TODO: Figure out a way to move this logic into
            # individual methods on Transaction that BaseTransaction and
            # NestedTransaction can override accordingly.
            if isinstance(transaction, BaseTransaction):
                continue
            else:
                raise
        finally:
            # Before we go, restore the current transaction to whatever it was
            # before we started.
            stm_state.current = transaction.parent


def retry():
    raise RetryLater


def or_else(*args):
    for function in args:
        # Try to run each function in sequence, in its own transaction so that
        # if it raises RetryLater (or any other exception) its effects will be
        # undone.
        try:
            return atomically(function)
        except RetryLater:
            # Requested a retry, so move on to the next alternative
            pass
    # All of the alternatives retried, so retry ourselves.
    retry()












































