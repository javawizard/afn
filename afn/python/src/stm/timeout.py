
from threading import Thread
import time
import stm

class Timeout(Exception):
    pass

# The current implementation spawns a thread that waits the specified number of
# seconds and then sets the variable it created to True. This is really
# suboptimal as it leaves threads around if something else causes the
# transaction using the timeout var to complete before the timeout happens. A
# better way to do this would be to just hold a weak reference to the var and
# then sleep for, say, 5 second intervals and check to see if the var's been
# collected yet, and bail if it has.

def make_timeout(seconds):
    """
    Creates and returns a TVar whose initial value is False. After the
    specified number of seconds (which can be fractional), the var's value will
    be set to True.
    
    This can be used to implement timeouts in transactions. For example, one
    could wait for an item to become available on a queue, timing out after,
    say, ten seconds, with the following:
    
    var = make_timeout(10)
    @atomically
    def _():
        if var.get():
            raise Timeout # Or some other exception
        try:
            return some_queue.get(block=False)
        except Empty:
            retry()
    
    Note that this function must be called outside of a transaction (indeed, it
    will throw an exception if called from within one). The reasons for this
    are somewhat involved, and I'll hopefully write more here about them soon.
    """
    if stm._stm_state.current:
        raise Exception("Timeout vars cannot be created inside a transaction")
    var = stm.atomically(lambda: stm.TVar(False))
    def run():
        time.sleep(seconds)
        stm.atomically(lambda: var.set(True))
    Thread(name="stm.timeout for %s seconds" % seconds, target=run).start()
    return var


def wait_for_true(var):
    """
    Waits for the specified var to become True (or any true value), retrying if
    it's False (or any non-true value).
    """
    if not var.get():
        stm.retry()


def with_timeout(seconds, function):
    """
    Alternative to stm.atomically that runs a transaction, throwing Timeout if
    it retries and does not resume and run to completion before the specified
    number of seconds are up.
    """
    var = make_timeout(seconds)
    def wait_then_timeout():
        wait_for_true(var)
        raise Timeout
    return stm.atomically(lambda: stm.or_else(wait_then_timeout, function))
        




