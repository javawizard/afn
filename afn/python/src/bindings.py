
from collections import namedtuple
from threading import local as Local
from abc import ABCMeta, abstractmethod
from weakref import ref as WeakRef

SetValue = namedtuple("SetValue", ["value"])
SetList = namedtuple("SetList", ["value"])
SetDict = namedtuple("SetDict", ["value"])
SetDictEntry = namedtuple("SetDictEntry", ["key", "value"])
RemoveDictEntry = namedtuple("RemoveDictEntry", ["key"])

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


class Sender(object):
    __metaclass__ = ABCMeta
    @abstractmethod
    def add_receiver(self, receiver, weak=False):
        pass
    
    @abstractmethod
    def remove_receiver(self, receiver):
        pass


class Receiver(object):
    __metaclass__ = ABCMeta
    
    @abstractmethod
    def receive(self, action):
        pass
    
    @abstractmethod
    def validate(self, action):
        pass


class BaseSender(Sender):
    __metaclass__ = ABCMeta
    @abstractmethod
    def _create_initial_action(self):
        pass
    
    def __init__(self):
        super(BaseSender, self).__init__()
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


class BaseReceiver(Receiver):
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
            if isinstance(self, Propagating):
                self._send_receive(action)
    
    def validate(self, action):
        if process(self):
            self._validate(action)
            if isinstance(self, Propagating):
                self._send_validate(action)


class Propagating(BaseSender, BaseReceiver):
    """
    An abstract class that subclasses both BaseSender and BaseReceiver, and
    indicates to them that they should manually propagate events. In other
    words, this class provides no actual functionality beyond signaling
    (through isinstance(self, Propagating)) to BaseSender and BaseReceiver that
    they should automatically propagate events.
    
    Note that the way in which I do this might change. I don't particularly
    like this approach, as I would like to have BaseSender and BaseReceiver
    totally ignorant of each other, but I can't figure out a good way to do
    this properly. So this is how I'm doing things for now.
    """


class BindCell(Propagating):
    def __init__(self, value, validator=None):
        super(BindCell, self).__init__()
        # Validate the initial value against the validator
        if validator is not None:
            validator(value)
        self._value = value
        self._validator = validator
    
    def _receive(self, action):
        check_type(action, SetValue)
        # Set our value
        self._value = action.value
    
    def _validate(self, action):
        check_type(action, SetValue)
        # Use our validator, if we have one, to validate the value
        if self._validator is not None:
            self._validator(action.value)
    
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


class _ValueTranslatorCell(Propagating):
    def __init__(self, converter):
        super(_ValueTranslatorCell, self).__init__()
        self._value = None
        self._converter = converter
    
    def _receive(self, action):
        check_type(action, SetValue)
        self._value = action.value
        # TODO: What should we do about circuit processing here? The problem is
        # that we need to prevent the two translator cells linked to each other
        # from updating each others' values in an infinite loop, and the only
        # way I can think of doing that right now is to update the other value
        # in the same circuit, and I still need to figure out whether that can
        # end up resulting in desyncs.
        # UPDATE: I've realized this can cause problems (scenario: a synthetic
        # dict providing a view of two ends of a value translator, will result
        # in two updates to the dict which both need to propagate properly), so
        # I've changed this to directly set the other translator's value in a
        # new circuit, mark the other translator as visited, and manually
        # propagate the change. This should probably be rewritten to use a
        # public-ish method on this class so that one instance doesn't need to
        # know about the internals of another instance, but I'll worry about
        # that later. 
        converted_value = self._converter(action.value)
        with circuit():
            process(self._other)
            self._other._value = converted_value
            self._other._send_receive(SetValue(converted_value))
    
    def _validate(self, action):
        check_type(action, SetValue)
        converted_value = self._converter(action.value)
        with circuit():
            process(self._other)
            self._other._send_validate(converted_value)
    
    def _create_initial_action(self):
        return SetValue(self._value)


class ValueTranslator(object):
    def __init__(self, a_to_b, b_to_a, a):
        self._a = _ValueTranslatorCell(a_to_b)
        self._b = _ValueTranslatorCell(b_to_a)
        self._a._other = self._b
        self._b._other = self._a
        with circuit():
            self._a.validate(SetValue(a))
        with circuit():
            self._a.receive(SetValue(a))
    
    @property
    def a(self):
        return self._a
    
    @property
    def b(self):
        return self._b


def value_bind(a, b):
    a.add_receiver(b)
    b.add_receiver(a)


class Dict(BaseSender, BaseReceiver):
    def __init__(self):
        super(Dict, self).__init__()
        self._dict = {}
    
    def _create_initial_action(self):
        return SetDict(self._dict.copy())
    
    






















