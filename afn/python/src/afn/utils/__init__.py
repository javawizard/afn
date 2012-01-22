"""
A collection of utilities used by lots of different programs and libraries in
the AFN project that don't really fit into just one of those programs/libraries.
Most (or maybe all... I'm not sure at the moment...) of these were written by
Alexander Boyd.
"""

from traceback import print_exc as _print_exc
import time
from threading import Thread
from functools import update_wrapper

def slicer(length, start=None, stop=None, step=None):
    """
    A generator that yields the indexes (in the correct order) that the
    specified Python list slice, evaluated over a sequence of the specified
    length, would produce.
    """
    start, stop, step = slice(start, stop, step).indices(length)
    if(step > 0):
        while start < stop:
            yield start
            start += step
    else:
        while start > stop:
            yield start
            start += step


def field(function):
    name = function.__name__
    def get_function(self):
        return getattr(self, "_p_" + name)
    def set_function(self, value):
        setattr(self, "_p_" + name, value)
    p = property(get_function, set_function, doc=function.__doc__)
    return p


def full_name(some_class):
    if not isinstance(some_class, type): # This is an instance of a class, not
        # a class itself, so we need to get this object's class
        some_class = type(some_class)
    return some_class.__module__ + "." + some_class.__name__


class BlankObject(object):
    """
    A blank object that allows arbitrary assignment to its instances'
    attributes.
    """
    pass


def cast(instance, *types):
    """
    Raises an exception if isinstance(instance, types) returns False. This is
    useful in, for example, Autobus interfaces to ensure that objects passed
    in by a client are of the correct type.
    """
    if not isinstance(instance, types):
        raise Exception("The specified object's type is " + str(type(instance))
                + ", but it needs to be one of " + str(types))


class NoExceptions(object):
    """
    A with-statement context manager that suppresses and silently discards all
    exceptions thrown from within it. You'll usually want to use the single
    instance of this class, afn.utils.no_exceptions.
    """
    def __enter__(self):
        pass
    
    def __exit__(self, *args):
        return True

no_exceptions = NoExceptions()


class Suppress(object):
    """
    A context manager that suppresses exceptions of a particular type. Such
    exceptions, when thrown from within the block using this context manager,
    will be silently discarded.
    """
    def __init__(self, suppress_type):
        self.suppress_type = suppress_type
    
    def __enter__(self):
        pass
    
    def __exit__(self, exception_type, *args):
        if exception_type:
            return issubclass(exception_type, self.suppress_type)


class Consume(object):
    """
    A context manager that accepts an exception type and a function as
    constructor arguments. If an exception is thrown from within a with
    statement using this context manager, and the specified function is
    actually a function (or some other type of callable), then the function
    is called, passing in the exception, and the exception is suppressed. If
    the function is not callable, the exception is propagated upward as normal.
    
    If the consume parameter is False, this context manager does nothing. This
    is used in a few cases in Autobus where whether or not a particular piece
    of code should be run under a Consume context manager is dependent on a
    particular argument to the function running the code; most of the functions
    in Autobus that accept a parameter named "safe" are implemented this way,
    with the value of this safe parameter being passed as the consume parameter.
    """
    def __init__(self, exception_type, function, consume):
        self.exception_type = exception_type
        self.function = function
        self.consume = consume
    
    def __enter__(self):
        pass
    
    def __exit__(self, t, v, tb):
        if not self.consume: # We're not supposed to do anything
            return
        if not t: # No exception was thrown
            return
        if not issubclass(t, self.exception_type): # Not the type of exception
            # we're looking for
            return
        if callable(self.function):
            self.function(v)
            return True


class PrintExceptions(object):
    """
    Similar to NoExceptions and no_exceptions, but prints (with
    traceback.print_exc) any exceptions that it suppresses.
    """
    def __enter__(self):
        pass
    
    def __exit__(self, t, v, tb):
        if tb:
            _print_exc()
        return True

print_exceptions = PrintExceptions()


def filter_dict(input, rule_map):
    """
    Creates a new dictionary containing one key for each key that's present in
    both input and rule_map, which should both be dictionaries. The key
    present in the output dictionary will be the value of the corresponding
    entry in rule_map.
    
    In other words, this copies input, removes all keys not present in
    rule_map, then renames them to be the values of the matching keys in
    rule_map.
    """
    new = {}
    for old_key, new_key in rule_map.items():
        if old_key in input:
            new[new_key] = input[old_key]
    return new


def at(seconds, function):
    """
    Starts a new thread that will run the specified function after the
    specified number of seconds. The function will only be run once; after
    running the function, the thread will die.
    """
    def run():
        time.sleep(seconds)
        function()
    Thread(name="afn.utils.at", target=run).start()


def print_on_fail(function):
    """
    A decorator that can be used thus:
    
    @print_on_fail
    def example(...):
        ...
    
    to call print_exc whenever an uncaught exception gets thrown out of the
    function. The exception will continue to propagate after the stack trace
    has been printed.
    """
    def wrapper(*args, **kwargs):
        try:
            return function(*args, **kwargs)
        except:
            _print_exc()
            raise
    update_wrapper(wrapper, function)
    return wrapper


def rotations(sequence):
    """
    rotations("ABC") -> ["ABC", "BCA", "CAB"]
    """
    return [(sequence[i:] + sequence[:i]) for i in range(len(sequence))]


def wrap(function):
    """
    Returns a newly-created function that simply calls the specified one with
    the specified arguments. The returned function behaves, when called, as if
    it was the function originally passed into the wrap function.
    
    The reason that this is useful is that accessing a bound method of a
    particular object actually returns a different bound method object each
    time, so tracking these in a list of, say, listeners doesn't work. The wrap
    function can be used to wrap a bound method in a function that won't
    change (and that, furthermore, is always hashable, whereas bound methods
    are only hashable if their underlying objects are also hashable), and the
    wrapper can then be used as the listener.
    """
    return lambda *args, **kwargs: function(*args, **kwargs)


def get_path(object, path):
    if isinstance(path, basestring):
        components = path.split(".")
    for component in components:
        object = getattr(object, component)
    return object

def print_args(*args, **kwargs):
    print args, kwargs






























