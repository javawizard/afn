
from traceback import print_exc
from afn.utils import print_exceptions

class Event(object):
    """
    An event manager. Listeners can be added by calling the listen function.
    Listeners can be removed by calling the unlisten function. The replace
    function removes all listeners and then adds one specific listener, in
    essence replacing all of the current listeners with the one specified.
    
    Calling an instance of this class fires the event. All arguments passed
    to the invocation will be passed to each listener. If any of the
    listeners raise an exception, the exception's traceback will be printed
    to stdout, but the exception will be otherwise suppressed (and in
    particular, all remaining listeners will still be called).
    
    NOTE: Event instances are not thread-safe. Some external synchronization
    mechanism must be used if Event instances are to be shared among threads.
    """
    def __init__(self):
        """
        Creates a new event.
        """
        self._listeners = []
    
    def listen(self, function):
        """
        Adds the specified function as a listener to this event. If the
        function is already present on this event, it will not be added twice.
        """
        if function not in self._listeners:
            self._listeners.append(function)
    
    def unlisten(self, function):
        """
        Removes the specified function from this event's list of listeners.
        """
        try:
            self._listeners.remove(function)
        except ValueError:
            pass
    
    def __call__(self, *args, **kwargs):
        """
        Fires all listeners registered to this event. The specified arguments
        (including keyword arguments) will be passed to all of the listeners.
        """
        for listener in self._listeners[:]: # Clone in case someone modifies us
            with print_exceptions:
                listener(*args, **kwargs)


class EventTable(object):
    """
    A table of events. This is similar to the Event class, but it allows
    different named events to be listened for and provided.
    
    When calling an instance of EventTable, the first argument should be the
    name of the event to fire. All remaining arguments, and all keyword
    arguments, are passed to the event listeners.
    
    NOTE: The same warning about thread-safety that applies to Event also
    applies to EventTable.
    """
    def __init__(self):
        self._table = {}
    
    def listen(self, name, function):
        """
        Adds a function, listening on the event with the specified name.
        """
        if name not in self._table:
            self._table = []
        if function not in self._table[name]:
            self._table[name].append(function)
    
    def unlisten(self, name, function):
        """
        Removes the specified function listening on the specified name.
        """
        try:
            self._table[name].remove(function)
            if not self._table[name]:
                del self._table[name]
        except (ValueError, KeyError):
            pass
    
    def __call__(self, *args, **kwargs):
        """
        Fires all listeners registered to the event named by the first argument,
        passing in all remaining arguments and all keyword arguments.
        """
        if len(args) < 1:
            raise Exception("EventTable instances must be called with at least "
                    "one argument, the name of the event to fire.")
        name = args[0]
        args = args[1:]
        for listener in self._table.get(name, []):
            with print_exceptions:
                listener(*args, **kwargs)


class Property(object):
    pass


class PropertyTable(object):
    def __init__(self):
        self._value_table = {} # Map of names to values
        self._watch_table = {} # Map of names to lists of watchers
    
    def watch(self, name, function, initial=True):
        # Create a list in the watch table if we don't already have one
        if name not in self._watch_table:
            self._watch_table[name] = []
        # If we're already watching the property, just return
        if function in self._watch_table[name]:
            return
        # Add the function to the list of watchers
        self._watch_table[name].append(function)
        # If initial is True, pass the property's current value to the function
        if initial:
            function(self._value_table.get(name, None))
    
    def unwatch(self, name, function, initial=True):
        # If we're present in the watchers for this property and initial is
        # True, call the function
        if (initial
                and name in self._watch_table
                and function in self._watch_table[name]):
            function(None)
        # Try to remove the watcher, and remove the list of watchers if it was
        # the last watcher on the property
        try:
            self._watch_table[name].remove(function)
            if not self._watch_table[name]:
                del self._watch_table[name]
        except (ValueError, KeyError):
            pass
    
    def get(self, name, default=None):
        # Just return the current value
        return self._value_table.get(name, default)
    
    def set(self, name, value):
        # Update the stored value
        self._value_table[name] = value
        # Notify all watchers on this property that it's changed
        for watcher in self._watch_table.get(name, []):
            with print_exceptions:
                watcher(value)
    
    def delete(self, name):
        # Delete the stored value
        del self._value_table[name]
        # Notify all watchers on this property that it's been deleted
        for watcher in self._watch_table.get(name, []):
            with print_exceptions:
                watcher(None)
    
    def __getitem__(self, name):
        if name not in self._value_table:
            raise KeyError(name)
        return self._value_table[name]
    __setitem__ = set
    __delitem__ = delete







