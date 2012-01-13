from __future__ import with_statement

from threading import Thread, RLock
import threading
from functools import partial, update_wrapper
import inspect
import sys
import traceback
import afn.utils

def as_new_thread(function): 
    """
    A decorator that causes the decorated function to be invoked in a new
    thread. The function returned will invoke the specified function on a
    newly-created thread. The target function's return value will be discarded;
    the resulting function will always return None.
    """
    def wrapper(*args, **kwargs):
        Thread(name="afn.utils.concurrent.as_new_thread", target=function, args=args, kwargs=kwargs).start()
    update_wrapper(wrapper, function)
    wrapper.wrapped = function
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
    
    Technically, the specified lock can be any object that can be passed as the
    context manager of a with statement. This behavior will be preserved
    throughout potential future revisions of this function/decorator.
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
    wrapper.wrapped = function
    wrapper.__doc__ = format_argspec(function) + "\n\n" + getdoc(function)
    return wrapper

def synchronized_on(attribute=None):
    """
    Similar to the synchronized function, but must be used as a decorator thus:
    
    @synchronized_on("some_attribute")
    def some_method(self, ...):
        ...
    
    Namely, the method being decorated must be just that: a method inside of a
    class. This will cause the method to be synchronized on the lock (or any
    other sort of context manager) located at self.some_attribute, which allows
    for methods to be synchronized on per-instance locks.
    
    some_attribute can contain dots; these will be parsed as per normal Python
    semantics, so, for example, @synchronized_on("x.y") synchronizes on
    self.x.y.
    """
    def decorator(function):
        def wrapper(self, *args, **kwargs):
            with afn.utils.get_path(self, attribute):
                return function(self, *args, **kwargs)
        update_wrapper(wrapper, function)
        wrapper.wrapped = function
        wrapper.__doc__ = format_argspec(function) + "\n\n" + getdoc(function)
        return wrapper
    return decorator

def format_argspec(function):
    argspec = inspect.getargspec(function)
    args = inspect.formatargspec(*argspec)
    return function.__name__ + args

def getdoc(function):
    doc = inspect.getdoc(function)
    if doc is None:
        doc = ""
    return doc

class AtomicInteger(object):
    """
    An integer or long (it switches between the two as needed) that provides
    a guarantee of atomicity across all operations it supports, and in
    particular, it guarantees that inline operations (such as += and *=) will
    be atomic. For example:
    
    number = AtomicInteger()
    number.set(1)
    number += 1
    number == 2
    """

    def __init__ (self, value=0):
        self.integer = value
        self.lock = RLock()
        return
    
    def get(self):
        with self.lock:
            return self.integer
    
    def get_and_add(self, value_to_add=1):
        with self.lock:
            value = self.integer
            self.integer += value_to_add
            return value
    
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


def dump_thread_traces():
    """
    Prints a traceback to stdout for each live thread in the Python interpreter.
    This is useful for when infinite loops or other strange behavior are
    occurring and you're not sure where; you could write some code to call this
    function when such a situation is detected or when the user sends a
    particular signal to the process.
    """
    threads = threading.enumerate()
    threads_by_id = dict((t.ident, t) for t in threads)
    stack_traces = sys._current_frames()
    print "*** STACK TRACES OF ALL LIVE THREADS ***"
    print ""
    for thread_id, frame in stack_traces.items():
        print "Stack trace for " + str(threads_by_id.get(thread_id, thread_id)) + ":"
        lines = traceback.format_list(traceback.extract_stack(frame))
        for line in lines:
            print line,
        print ""
    print "*** END STACK TRACES ***"

















