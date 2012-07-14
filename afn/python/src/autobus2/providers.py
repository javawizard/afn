
from autobus2.service import ServiceProvider
from afn.utils.listener import Event, EventTable, PropertyTable
from autobus2 import constants

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
    


    


