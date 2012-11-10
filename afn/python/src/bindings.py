
from collections import namedtuple
from threading import local as Local
from abc import ABCMeta, abstractmethod
from weakref import ref as WeakRef

SetValue = namedtuple("SetValue", ["value"])
SetList = namedtuple("SetList", ["value"])
SetDict = namedtuple("SetDict", ["value"])

context_thread_local = Local()

class StrongRef(object):
    """
    A class that behaves similarly to weakref.ref, except that it maintains a
    strong reference to the underlying object. It exists mainly so that code
    in BaseReceiver can function exactly the same for weak references as for
    strong references, without a bunch of "if isinstance(value, weakref.ref)"
    statements.
    
    Note that the StrongRef constructor does not accept a callback like
    weakref.ref does.
    """
    def __init__(self, value):
        self.__value = value
    
    def __call__(self):
        return self.__value


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
    def add_receiver(self, receiver, weak=False):
        pass
    
    @abstractmethod
    def remove_receiver(self, receiver):
        pass


class BaseSender(ValueSender):
    __metaclass__ = ABCMeta
    @abstractmethod
    def _create_initial_action(self):
        pass
    
    def __init__(self):
        self.__receivers = []
    
    def add_receiver(self, receiver, weak=False):
        # Create the initial action and validate it against the new receiver
        initial_action = self._create_initial_action()
        with circuit():
            receiver.validate(initial_action)
        # Create a ref to store this receiver in
        if weak:
            ref = WeakRef(receiver, self.__handle_dereference)
        else:
            ref = StrongRef(receiver)
        self.__receivers.append(ref)
        # Actually send out the initial value
        with circuit():
            receiver.receive(initial_action)
    
    def __handle_dereference(self, weakref):
        # Find the weakref in the list, if it's still there, and remove it.
        for index, ref in enumerate(self.__receivers):
            if ref is weakref:
                del self.__receivers[index]
                break
    
    def remove_receiver(self, receiver):
        # Find a ref whose value is the receiver in the list, and remove it.
        for index, ref in enumerate(self.__receivers):
            if ref() == receiver:
                del self.__receivers[index]
                break
    
    def _send_validate(self, action):
        for ref in self.__receivers:
            receiver = ref()
            if receiver is not None:
                receiver.validate(action)
    
    def _send_receive(self, action):
        for ref in self.__receivers:
            receiver = ref()
            if receiver is not None:
                receiver.receive(action)


class ValueReceiver(object):
    __metaclass__ = ABCMeta
    
    @abstractmethod
    def receive(self, action):
        pass
    
    @abstractmethod
    def validate(self, action):
        pass


class BaseReceiver(ValueReceiver):
    __metaclass__ = ABCMeta
    
    @abstractmethod
    def _receive(self, action):
        pass
    
    @abstractmethod
    def _validate(self, action):
        pass
    
    def receive(self, action):
        if process(self):
            self._receive(action)
    
    def validate(self, action):
        if process(self):
            self._validate(action)


class BindCell(BaseSender, BaseReceiver):
    def __init__(self, value, validator=None):
        # Validate the initial value against the validator
        if validator is not None:
            validator(value)
        self._value = value
        self._validator = validator
    
    def _receive(self, action):
        check_type(action, SetValue)
        # Set our value
        self._value = action.value
        # Propagate the value
        self._send_receive(action)
    
    def _validate(self, action):
        check_type(action, SetValue)
        # Use our validator, if we have one, to validate the value
        if self._validator is not None:
            self._validator(action.value)
        # Propagate the validation
        self._send_validate(action)
    
    def _create_initial_action(self):
        return SetValue(self._value)
    
    @property
    def value(self):
        return self._value
    
    @value.setter
    def value(self, new_value):
        with circuit():
            self.validate(SetValue(new_value))
        with circuit():
            self.receive(SetValue(new_value))


class _ValueTranslatorCell(BaseSender, BaseReceiver):
    def __init__(self, converter):
        self._value = None
        self._converter = converter
    
    def _receive(self, action):
        check_type(action, SetValue)
        self._value = action.value
        self._send_receive(action)
        # TODO: What should we do about circuit processing here? The problem is
        # that we need to prevent the two translator cells linked to each other
        # from updating each others' values in an infinite loop, and the only
        # way I can think of doing that right now is to update the other value
        # in the same circuit, and I still need to figure out whether that can
        # end up resulting in desyncs. 
        self._other.receive(SetValue(self._converter(action.value)))
    
    def _validate(self, action):
        check_type(action, SetValue)
        self._send_validate(action)
        self._other.validate(SetValue(self._converter(action.value)))


class ValueTranslator(object):
    def __init__(self, a_to_b, b_to_a, a):
        self._a = _ValueTranslatorCell(self._a_to_b)
        self._b = _ValueTranslatorCell(self._b_to_a)
        self._a._other = self._b
        self._b._other = self._a
        with circuit():
            self._a.validate(a)
        with circuit():
            self._a.receive(a)
    
    @property
    def a(self):
        return self._a
    
    @property
    def b(self):
        return self._b





















