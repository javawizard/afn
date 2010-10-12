from threading import Thread, RLock
from functools import partial, update_wrapper

def as_new_thread(function):
    """
    A decorator that causes the decorated function to be invoked in a new
    thread.
    """
    def wrapper(*args, **kwargs):
        Thread(target=function, args=args, kwargs=kwargs).start()
    update_wrapper(wrapper, function)
    return wrapper

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
            return function(*args, **kwargs)
    update_wrapper(wrapper, function)
    return wrapper

class AtomicInteger(object):
    """
    An integer or long (it switches between the two as needed) that provides
    a guarantee of atomicity across all operations it supports. For example:
    
    number = AtomicInteger()
    number.set(1)
    number += 1
    number == 2
    """

    def __init__ (self):
        self.integer = 0
        self.lock = RLock()
        return
    
    def get(self):
        with self.lock:
            return self.integer
    
    def set(self, integer):
        with self.lock:
            self.integer = integer
    
    def __cmp__(self, other):
        with self.lock:
            return self.integer.__cmp__(other)
    
    def __hash__(self):
        with self.lock:
            return self.integer.__hash__()
    
    def __nonzero__(self):
        with self.lock:
            return self.integer.__nonzero__()
    
    def __repr__(self):
        return str(self.integer)
    
    def __int__(self):
        return int(self.integer)
    
    def __long__(self):
        return long(self.integer)
    
    def __iadd__(self, other):
        with self.lock:
            self.integer = self.integer.__add__(other)
            return self
    
    def __isub__(self, other):
        with self.lock:
            self.integer = self.integer.__sub__(other)
            return self
    
    def __imul__(self, other):
        with self.lock:
            self.integer = self.integer.__mul__(other)
            return self
    
    def __idiv__(self, other):
        with self.lock:
            self.integer = self.integer.__div__(other)
            return self
    
    def __itruediv__(self, other):
        with self.lock:
            self.integer = self.integer.__truediv__(other)
            return self
    
    def __ifloordiv__(self, other):
        with self.lock:
            self.integer = self.integer.__floordiv__(other)
            return self
    
    def __imod__(self, other):
        with self.lock:
            self.integer = self.integer.__mod__(other)
            return self
    
    def __ipow__(self, other):
        with self.lock:
            self.integer = self.integer.__pow__(other)
            return self
    
    def __ilshift__(self, other):
        with self.lock:
            self.integer = self.integer.__lshift__(other)
            return self
    
    def __irshift__(self, other):
        with self.lock:
            self.integer = self.integer.__rshift__(other)
            return self
    
    def __iand__(self, other):
        with self.lock:
            self.integer = self.integer.__and__(other)
            return self
    
    def __ixor__(self, other):
        with self.lock:
            self.integer = self.integer.__xor__(other)
            return self
    
    def __ior__(self, other):
        with self.lock:
            self.integer = self.integer.__or__(other)
            return self
