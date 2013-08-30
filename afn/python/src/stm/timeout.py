
from threading import Thread
import time
import stm
import weakref

class Timeout(Exception):
    pass


class _TimeoutThread(Thread):
    def __init__(self, var, stop_time):
        Thread.__init__(self)
        self.var_ref = weakref.ref(var)
        self.stop_time = stop_time
    
    def run(self):
        # There's no platform agnostic way to sleep in such a way that one can
        # be interrupted half-way through (indeed, threading.Condition
        # basically uses a busywait with exponentially increasing delays), so
        # we'll do the next best thing: check every second to see if we've lost
        # our reference to the variable to change and return if we did.
        # 
        # The main upside of this is that we guarantee that the transaction
        # will be interrupted precisely when the timeout expires, not up to
        # 50 milliseconds later as is possible with threading.Condition, but it
        # does mean that spare threads can hang around for up to a second if
        # a transaction returns on its own without a timeout happening.
        # 
        # At some point I'd like to write a ctypes wrapper around pthread,
        # which is used on every platform except Windows and which does have
        # proper timeout support, that's used on platforms that support it to
        # get rid of this delay, but it's not top priority right now.
        while time.time() < self.stop_time:
            # Sleep up to 1 second or the amount of time remaining until we're
            # supposed to time out, whichever is less
            time.sleep(min(1, max(0, self.stop_time - time.time())))
            if not self.var_ref():
                # Variable was garbage collected since a second ago, so return
                return
        var = self.var_ref()
        if var:
            # Still have a reference to the variable, so set it to True.
            stm.atomically(lambda: var.set(True))


def make_delay(seconds):
    """
    Creates and returns a TVar whose initial value is False. After the
    specified number of seconds (which can be fractional), the var's value will
    be set to True.
    
    This can be used to implement timeouts in transactions. For example, one
    could wait for an item to become available on a queue, timing out after,
    say, ten seconds, with the following:
    
    var = make_delay(10)
    def get_item():
        try:
            return some_queue.get(block=False)
        except Empty:
            if var.get():
                raise Timeout # Or some other exception
            else:
                retry()
    item = atomically(get_item)
    
    Note that this function must be called outside of a transaction (indeed, it
    will throw an exception if called from within one). The reasons for this
    are somewhat involved, and I'll hopefully write more here about them soon.
    """
    return make_timeout(time.time() + seconds)


def make_timeout(stop_time):
    """
    A variant of make_delay that sets the returned TVar to True when
    time.time() becomes greater than stop_time.
    """
    if stm._stm_state.current:
        raise Exception("Timeout vars cannot be created inside a transaction")
    var = stm.atomically(lambda: stm.TVar(False))
    _TimeoutThread(var, stop_time).start()
    return var


def wait_for_true(var):
    """
    Waits for the specified var to become True (or any true value), retrying if
    it's False (or any non-true value).
    """
    if not var.get():
        stm.retry()


def wait_then_raise(var, exception_type=Timeout):
    """
    Like wait_for_true(var), but raise a new instance of the specified
    exception type once the specified var becomes True. This is used primarily
    by with_delay to handle raising Timeout if the transaction doesn't retry
    soon enough.
    """
    wait_for_true(var)
    raise exception_type()


def with_delay(seconds, function):
    """
    Alternative to stm.atomically that runs a transaction, throwing Timeout if
    it retries and does not resume and run to completion before the specified
    number of seconds are up.
    """
    return with_timeout(time.time() + seconds, function)


def with_timeout(stop_time, function):
    """
    A variant of with_delay that accepts its timeout as a number of seconds
    since the epoch at which the transaction should raise Timeout. 
    """
    var = make_timeout(stop_time)
    return stm.atomically(lambda: stm.or_else(function, lambda: wait_then_raise(var)))
        




