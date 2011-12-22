
from traceback import print_exc as _print_exc
import time
from threading import Thread
from functools import update_wrapper

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
    def __enter__(self):
        pass
    
    def __exit__(self, *args):
        return True

no_exceptions = NoExceptions()


class Suppress(object):
    def __init__(self, suppress_type):
        self.suppress_type = suppress_type
    
    def __enter__(self):
        pass
    
    def __exit__(self, exception_type, *args):
        return issubclass(type(exception_type), self.suppress_type)


class PrintExceptions(object):
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
    Thread(target=run).start()

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








