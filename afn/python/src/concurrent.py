
from threading import RLock
from functools import partial, update_wrapper

def synchronized(lock=None, function=None):
    """
    Returns a function wrapping the specified function that locks on the
    specified lock before calling the function and releases it after the 
    function has been invoked. Omitting the second argument will return
    functools.partial(synchronized, lock), and specifying a callable lock will
    instead return synchronized(None, lock), which effectively allows this
    function to be used as a decorator thus:
    
    @synchronized(some_lock)
    @synchronized()
    @synchronized
    
    All three uses are valid, with the latter two creating a new reentrant
    lock for this function.
    """
    if callable(lock): # in case it's used as @synchronized
        return synchronized(None, lock)
    if function is None:
        return partial(synchronized, lock)
    if lock is None:
        lock = RLock()
    def wrapper(*args, **kwargs):
        with lock:
            function(*args, **kwargs)
    update_wrapper(wrapper, function)
    return wrapper

class AtomicInteger(object):
    """
    An integer or long (it switches between the two as needed) that provides
    a guarantee of atomicity across all operations it supports. For example:
    
    number = AtomicInteger()
    number.set(1)
    
    TODO: finish this up
    """