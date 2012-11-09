
from collections import namedtuple
from threading import local as Local
from abc import ABCMeta, abstractmethod

SetValue = namedtuple("SetValue", ["value"])
SetList = namedtuple("SetList", ["value"])
SetDict = namedtuple("SetDict", ["value"])

context_thread_local = Local()


class ValidationFailed(Exception):
    pass


def check_type(value, type):
    if not isinstance(value, type):
        raise Exception("Value %r is not an instance of %r" % (value, type))


class CircuitContext(object):
    """
    A context manager that, upon entering, sets the current circuit to a
    newly-created circuit and pushes the formerly-current circuit onto a stack.
    Upon exiting, the former circuit will be restored, if there was any.
    """
    def __enter__(self):
        stack = context_thread_local.__dict__.setdefault("circuit_stack", [])
        stack.append(set())
    
    def __exit__(self, *args):
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
    __metaclass__ = ABCMeta
    @abstractmethod
    def add_receiver(self, receiver):
        pass
    
    @abstractmethod
    def remove_receiver(self, receiver):
        pass


class ValueReceiver(object):
    __metaclass__ = ABCMeta
    
    @abstractmethod
    def receive(self, action):
        pass
    
    @abstractmethod
    def validate(self, action):
        pass


class BindCell(ValueSender, ValueReceiver):
    def __init__(self, value, validator=None):
        self._value = value
        self._validator = validator
        self._receivers = []
    
    def receive(self, action):
        if process(self):
            check_type(action, SetValue)
            # Set our value
            self._value = action.value
            # Propagate the value
            for receiver in self._receivers:
                receiver.receive(action)
    
    def validate(self, action):
        if process(self):
            check_type(action, SetValue)
            # Use our validator, if we have one, to validate the value
            if self._validator is not None:
                self.validator.validate(action)
            # Propagate the validation
            for receiver in self._receivers:
                receiver.validate(action)
    
    def add_receiver(self, receiver):
        # Validate our current value against the new receiver first; if our
        # value isn't valid, the exception will propagate out, preventing us
        # from binding, which is what we want
        with circuit():
            receiver.validate(SetValue(self._value))
        # The value passed validation, so add the receiver and propagate the
        # value.
        self._receivers.append(receiver)
        with circuit():
            receiver.receive(SetValue(self._value))
    
    def remove_receiver(self, receiver):
        self._receivers.remove(receiver)
    
    @property
    def value(self):
        return self._value
    
    @value.setter
    def value(self, new_value):
        with circuit():
            self.validate(SetValue(new_value))
        with circuit():
            self.receive(SetValue(new_value))





















