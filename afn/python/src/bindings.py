
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

circuit = CircuitContext


def process(object):
    """
    Checks to see if the specified object is in the current circuit, i.e. it
    has already been processed. If so, False is returned, indicating that the
    specified object does not need to be processed again. If the specified
    object is not in the current circuit, the object is added to the circuit,
    and True is returned. Thus one can implement receive or validate methods
    like so:
    
    def receive(self, action):
        if process(self):
            ...process the value, propagate to other receivers...
    
    and the circuit system will ensure that the object doesn't process the same
    action twice.
    
    Note that if there isn't a current circuit (i.e. if this isn't being called
    from within a block using a CircuitContext manager), an exception will be
    thrown.
    """
    if not hasattr(context_thread_local, "circuit_stack"):
        raise Exception("process() can only be called within a CircuitContext "
                "manager")
    circuit = context_thread_local.circuit_stack[-1]
    if object in circuit: # Already processed, so return False
        return False
    # Not processed; add to circuit, then return True
    circuit.add(object)
    return True


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
        

