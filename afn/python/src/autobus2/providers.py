
from autobus2.service import ServiceProvider
from afn.utils.listener import Event, EventTable, PropertyTable
from autobus2 import constants
from autobus2 import exceptions
from abc import ABCMeta, abstractmethod
from afn.utils.partial import Partial
from functools import wraps

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
    TODO: The documentation for this class needs to be rewritten as its
    functionality has changed quite a bit.
    """
    
    def __init__(self, only_decorated=False):
        BaseServiceProvider.__init__(self)
        # only_decorated means only publish functions decorated with the
        # publish function, which sets _autobus_publish to True
        self.only_decorated = only_decorated
        # Check all of our attributes
        for attr in dir(self):
            if attr.startswith("_"): # Skip underscore attributes
                continue
            # See if the attribute is a PyEvent or PyObject descriptor. We have
            # to check on the class for this as the instance will just return
            # the descriptor's value.
            class_value = getattr(type(self), attr, None)
            if isinstance(class_value, PyEvent):
                # Event descriptor; add it to the events table. Check to make
                # sure the correct name was assigned to the descriptor.
                if class_value.name != attr:
                    raise Exception("Mismatched event descriptor %r with "
                            "reported name %r but actual name %r on %r" % (
                                    class_value, class_value.name, attr, self))
                self._events[attr] = {"doc": class_value.doc}
                continue
            if isinstance(class_value, PyObject):
                # Object descriptor; add it to the objects table. Check to make
                # sure the correct name was assigned to the descriptor.
                if class_value.name != attr:
                    raise Exception("Mismatched object descriptor %r with "
                            "reported name %r but actual name %r on %r" % (
                                    class_value, class_value.name, attr, self))
                self._objects[attr] = {"doc": class_value.doc}
                continue
            # Wasn't an event descriptor or an object descriptor. Check to see
            # if it's a function.
            value = getattr(self, attr)
            # If it's a function, and it's either got _autobus_publish or we're
            # not just publishing decoreated functions, add it to the function
            # table.
            if callable(value) and (getattr(value, "_autobus_publish", False) or not only_decorated):
                self._functions[attr] = {}
    
    def __autobus_call__(self, name, args):
        if name.startswith("_"): # Don't allow functions whose names start
            # with underscores to be called remotely
            raise exceptions.NoSuchFunctionException("%s starts with an underscore" % name)
        try:
            # Make sure the function's in our function table; this will raise a
            # KeyError if it's not. We do this to prevent calling functions
            # that aren't marked _autobus_publish on services set to only
            # publish decorated functions.
            self._functions[name]
            # Now make sure we've got an attribute for the function so that we
            # can actually call it.
            function = getattr(self, name, None)
        except (AttributeError, KeyError):
            raise exceptions.NoSuchFunctionException("%s does not exist" % name)
        if not callable(function):
            raise exceptions.NoSuchFunctionException("%s exists but is not a function" % name)
        return function(*args)

# TODO: These should be descriptors used as class attributes that store things
# in some sort of _pyservice_events and _pyservice_objects dict or something,
# and then override accessing them to update the internal object/event
# dictionaries accordingly. (Or they could just use those dicts to store
# values and such, and not even need internal storage dicts. That's rather
# brilliant.) They should accept documentation strings, and since descriptors,
# as far as I've worked out, can't tell what variable they've been assigned
# into, they'll probably need parameters specifying their name, unless I have
# PyServiceProvider's __init__ scan the class's attributes and let the
# descriptors know what their names are. Or I could use a metaclass and have it
# do the scanning when it's initialized, but I'd really like to avoid using
# metaclasses if I don't absolutely have to.



def publish(function):
    function._autobus_publish = True
    return function


class PyEvent(object):
    """
    An event descriptor. Instances of this class can be used as descriptors on
    subclasses of PyServiceProvider to indicate an event field. The
    corresponding properties available on instances are read-only functions;
    calling them fires the event. For example:
    
    class MyService(PyServiceProvider):
        some_event = PyEvent("some_event", "This is a test event.")
        other_event = PyEvent("other_event", "This is another event.")
        ...other functions, events, and objects...
    service = MyService()
    ...publish service using autobus2.Bus.create_service...
    service.some_event("first arg", "second arg", "third arg")
    service.other_event("first arg", "second arg", "third arg")
    """
    def __init__(self, name, doc=None):
        """
        Creates a new PyEvent. name is the name of the attribute to which the
        PyEvent instance will be assigned. (This is needed as Python doesn't
        provide a simple way to find out what attribute a descriptor has been
        assigned to short of iterating over all of the class's attributes, and
        that would make performance dismal.) doc is the documentation for the
        event.
        """
        self.name = name
        self.doc = doc
    
    def __get__(self, instance, cls=None):
        """
        Returns afn.utils.partial.Partial(self.fire_event, instance). The
        resulting Partial instance, when called, will call fire_event, which
        will cause the event to be fired.
        """
        return Partial(self.fire_event, instance)
    
    def fire_event(*args):
        """
        Fires the event. The first argument is obviously self. The second
        argument is the instance of PyServiceProvider on which the event is to
        be fired. The remaining arguments are the arguments to pass to the
        event.
        
        This is primarily used by __get__; an instance of this method, wrapped
        in a Partial instance to partial in the PyServiceEvent, is returned
        from __get__.
        """
        self = args[0]
        instance = args[1]
        args = args[2:]
        instance._event_table(self.name, args)
    
    def __set__(self, instance, value):
        """
        Throws an exception (a TypeError right now; I may change this to an
        AttributeError or another type of exception later). Event descriptors
        can't be written; to fire the event, get its value and call it instead.
        (See __get__ and fire_event for more information.)
        """
        # TODO: would AttributeError be more appropriate?
        raise TypeError("Can't modify event %r.s" % (instance, self.name))


class PyObject(object):
    """
    Same as PyEvent, but creates an object. To set the object's value or get
    its current value, simply set or get the corresponding instance attribute.
    This attribute will default to None if it has not yet been assigned.
    """
    def __init__(self, name, doc=None):
        """
        Creates a new PyObject descriptor. Name is the name of the attribute
        to which this descriptor will be assigned; see PyEvent.__init__ for why
        this is needed. doc is the documentation for this object.
        """
        self.name = name
        self.doc = doc
    
    def __get__(self, instance, cls=None):
        """
        Returns the value of this object on the specified instance. This
        pretty much delegates straight to instance._object_values[self.name].
        """
        return instance._object_values.get(self.name, None)
    
    def __set__(self, instance, value):
        """
        Sets the value of this object on the specified instance. This pretty
        much delegates straight to instance._object_values[self.name] = value.
        """
        instance._object_values[self.name] = value
    
    






