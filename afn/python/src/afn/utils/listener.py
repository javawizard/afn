
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
    """
    def __init__(self):
        """
        Creates a new event.
        """
        self.listeners = []
    
    def listen(self, function):
        """
        Adds the specified function as a listener to this event. If the
        function is already present on this event, it will not be added twice.
        """
        if function not in self.listeners:
            self.listeners.append(function)
    
    def unlisten(self, function):
        """
        Removes the specified function from this event's list of listeners.
        """
        try:
            self.listeners.remove(function)
        except ValueError:
            pass
    
    def __call__(self, *args, **kwargs):
        """
        Fires all listeners registered to this event. The specified arguments
        (including keyword arguments) will be passed to all of the listeners.
        """
        for listener in self.listeners[:]: # Clone in case someone modifies us
            try:
                listener(*args, **kwargs)
            except:
                print_exc()
