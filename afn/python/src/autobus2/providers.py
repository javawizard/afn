
from autobus2.service import ServiceProvider
from afn.utils.listener import Event, EventTable, PropertyTable
from autobus2 import constants
from autobus2 import exceptions
from abc import ABCMeta, abstractmethod

class BaseServiceProvider(ServiceProvider):
    """
    A ServiceProvider that provides basic implementations of its methods. It
    allows functions to be registered and unregistered, and so on for events
    and objects. It handles automatically notifying any registered listeners of
    changes that have happened.
    
    The attributes _functions, _events, _event_table, _objects, and
    _object_values form part of the public interface of this class.
    """
    def __init__(self):
        self.__event = Event()
        self._functions = PropertyTable()
        self._functions.global_watch(self.__function_listener)
        self._events = PropertyTable()
        self._events.global_watch(self.__event_listener)
        self._event_table = EventTable()
        self._event_table.global_listen(self.__event_table_listener)
        self._objects = PropertyTable()
        self._objects.global_watch(self.__object_listener)
        self._object_values = PropertyTable()
        self._object_values.global_watch(self.__object_values_listener)
        raise NotImplementedError
    
    def __function_listener(self, name, old, new):
        if old is not None and new is not None: # Info change
            self.__event(constants.FUNCTION_UPDATED, name, new)
        elif old is not None: # Function was removed
            self.__event(constants.FUNCTION_REMOVED, name)
        elif new is not None: # Function was added
            self.__event(constants.FUNCTION_ADDED, name, new)
    
    def __event_listener(self, name, old, new):
        if old is not None and new is not None: # Info change
            self.__event(constants.EVENT_UPDATED, name, new)
        elif old is not None: # Event was removed
            self.__event(constants.EVENT_REMOVED, name)
        elif new is not None: # Event was added
            self.__event(constants.EVENT_ADDED, name, new)
    
    def __object_listener(self, name, old, new):
        if old is not None and new is not None: # Info change
            self.__event(constants.OBJECT_UPDATED, name, new)
        elif old is not None: # Object was removed
            self.__event(constants.OBJECT_REMOVED, name)
        elif new is not None: # Object was added
            self.__event(constants.OBJECT_ADDED, name, new, self._object_values.get(name, None))
    
    def __object_values_listener(self, name, old, new):
        # Only issue object change notifications if the object actually exists
        # in our _objects property table
        if not name in self._objects:
            return
        self.__event(constants.OBJECT_CHANGED, name, new)
    
    def __event_table_listener(self, *args):
        name = args[0]
        args = args[1:]
        if name in self._events:
            self.__event(constants.EVENT_FIRED, name, args)
    
    def __autobus_policy__(self, name):
        return constants.THREAD
    
    def __autobus_listen__(self, listener):
        for name, info in self._functions.items():
            listener(constants.FUNCTION_ADDED, name, info)
        for name, info in self._events.items():
            listener(constants.EVENT_ADDED, name, info)
        for name, info in self._objects.items():
            listener(constants.OBJECT_ADDED, name, info, self._object_values.get(name, None))
        self.__event.listen(listener)
    
    def __autobus_unlisten__(self, listener):
        self.__event.unlisten(listener)
        for name in self._functions:
            listener(constants.FUNCTION_REMOVED, name)
        for name in self._events:
            listener(constants.EVENT_REMOVED, name)
        for name in self._objects:
            listener(constants.OBJECT_REMOVED, name)


class PyServiceProvider(BaseServiceProvider):
    """
    A class that can be either extended or used as-is. It exposes all
    functions, both ones defined on subclasses of this class and ones assigned
    as the values of fields of instances of this class or its subclasses, as
    Autobus functions, all instances of afn.utils.listener.Event as events, and
    both fields assigned in the usual manner and fields whose values are
    instances of afn.utils.listener.Property  as Autobus objects.
    
    Thus one could construct a minimal service provider providing a single
    function thus:
    
    class Example(PyServiceProvider):
        def greet(self, name):
            return "Hello, %s!" % name
    
    And one could define a service provider using functions, events, and
    objects this:
    
    class Message(PyServiceProvider):
        def__init__(self):
            self.current_message = None
            self.message_changed = Event()
            
        def set_message(message):
            self.current_message = message
            self.message_changed(message)
    
    This provider would provide a function set_message, an event
    message_changed, and an object current_message.
    
    Note that functions, events, and objects whose names start with an
    underscore will not be published. This can be used to maintain internal
    variables that should not be published as objects.
    
    TODO: Decide whether all fields should be published or just those marked
    with a certain class-level descriptor. Said descriptor could then be used
    to specify the object's documentation. The same should be thought about for
    events.
    """
    
    def __init__(self):
        BaseServiceProvider.__init__(self)
        for attr in dir(self):
            if attr.startswith("_"):
                continue
            if callable(attr):
                self._functions[attr] = getattr(self, attr)
    
    def __autobus_call__(self, name, args):
        if name.startswith("_"): # Don't allow functions whose names start
            # with underscores to be called remotely
            raise exceptions.NoSuchFunctionException("%s starts with an underscore" % name)
        try:
            function = getattr(self, name, None)
        except AttributeError:
            raise exceptions.NoSuchFunctionException("%s does not exist" % name)
        if not callable(function):
            raise exceptions.NoSuchFunctionException("%s exists but is not a function" % name)
        return function(*args)
    
    






