
from collections import namedtuple
from threading import local as Local

ValueSet = namedtuple("ValueSet", ["value"])

context_thread_local = Local()

class CircuitContext(object):
    """
    A context manager that, upon entering, sets the current circuit to a
    newly-created circuit and pushes the formerly-current circuit onto a stack.
    Upon exiting, the former circuit will be restored, if there was any.
    """
    def __enter__(self):
        stack = context_thread_local.__dict__.setdefault("circuit_stack", [])
        stack.append(set())
    
    def __exit__(self):
        stack = context_thread_local.circuit_stack
        del stack[-1]
        if not stack:
            del context_thread_local.circuit_stack


class ProcessingContext(object):
    """
    A context manager that, upon entering, 
    """
    pass


circuit = CircuitContext
processing = ProcessingContext


def processed(object):
    """
    Returns true if the specified object has been processed in the current
    circuit, or false if it hasn't. Note that this will throw an exception if
    there isn't a current circuit. (It's considered an error to invoke
    validate or receive without a circuit, so this is usually what you want.)
    """


class ValueSender(object):
    def add_receiver(self, receiver):
        pass
    
    def remove_receiver(self, receiver):
        pass


class ValueReceiver(object):
    def receive(self, action):
        pass
    
    def validate(self, action):
        pass


class BindCell(ValueSender, ValueReceiver):
    def __init__(self, value):
        self._value = value
        self._receivers = []
    
    def receive(self, action):
        

