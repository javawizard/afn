
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
    """
    def __init__(self):
        self.__event = Event()
        raise NotImplementedError
    
    def __autobus_policy__(self, name):
        return constants.THREAD
    
    def __autobus_listen__(self, listener):
        self.__event.listen(listener)
    
    def __autobus_unlisten__(self, listener):
        self.__event.unlisten(listener)
    
    def _add_function(self, name, info):
        pass


class PyServiceProvider1(ServiceProvider):
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
        self.__event = Event()
    
    def __autobus_policy__(self, name):
        return constants.SYNC
    
    def __autobus_listen__(self, listener):
        self.__event.listen(listener)
    
    def __autobus_unlisten(self, listener):
        self.__event.unlisten(listener)
    
    def __autobus_call__(self, name, args):
        if name.startswith("_"): # Don't allow functions whose names start
            # with underscores to be called remotely
            raise exceptions.NoSuchFunctionException(name)
    


    


