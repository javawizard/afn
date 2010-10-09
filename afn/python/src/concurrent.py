
from threading import RLock
from functools import partial, update_wrapper

def synchronized(lock=None, function=None):
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