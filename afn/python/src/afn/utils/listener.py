
from traceback import print_exc

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
            try:
                listener(*args, **kwargs)
            except:
                print_exc()


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
            try:
                listener(*args, **kwargs)
            except:
                print_exc()







