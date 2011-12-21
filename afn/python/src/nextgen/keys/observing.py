
from afn.utils import singleton

# _NO_VALUE = singleton.Singleton("nextgen.keys.observing._NO_VALUE")

class Observable(object):
    def _get_current_values(self, value_list):
        pass
    
    def _init_observable(self):
        try:
            self._observers
        except AttributeError:
            self._observers = {}
            self._recursive_observer_count = 0
            # We're defining this here instead of as a normal class method
            # because bound methods (i.e. some_instance.some_method) can change
            # ids, but functions defined like this can't, and we need the same
            # id so that things we observe know when we're removing ourselves
            # as an observer
            def _recursive_listener_function(path, hierarchy, changes, context):
                for registration in self._observers.values():
                    if registration["recursive"]:
                        registration["observer"](self, changes)
            self._recursive_listener = _recursive_listener_function
    
    def observe(self, observer, recursive=False, context=None):
        self._init_observable()
        if id(observer) in self._observers:
            raise ValueError("The specified observer has already been added to "
                             "this object.")
        registration = {"observer": observer, "recursive": recursive, "context": context}
        self._observers[id(observer)] = registration
        if recursive:
            self._recursive_observer_count += 1
            if self._recursive_observer_count == 1: # This was the first
                # recursive observer to be added, so we need to go actually
                # listen to all of our current children for changes
                values = []
                self._get_current_values(values)
                for value in values:
                    if isinstance(value, Observable):
                        value.observe(self._recursive_listener, True)
    
    def unobserve(self, observer):
        self._init_observable()
        if id(observer) not in self._observers:
            raise ValueError("The specified observer has not been added as to "
                             "this object.")
        registration = self._observers[id(observer)]
        # Remove the actual registration
        del self._observers[id(observer)]
        # Then see if it was recursive
        if registration["recursive"]:
            self._recursive_observer_count -= 1
            if self._recursive_observer_count == 0: # This was the last
                # recursive observer to be removed, so we're going to go remove
                # the listeners we've placed on all of our children 
                values = []
                self._get_current_values(values)
                for value in values:
                    if isinstance(value, Observable):
                        value.unobserve(self._recursive_listener)
    
    def _notify_changed(self, changes):
        if len(changes) == 0:
            return
        self._init_observable()
        path = []
        hierarchy = [self]
        # See if we're recursively listening at present
        if self._recursive_observer_count > 0:
            # If we are, see if this change adds or removes any values
            for change in changes:
                if hasattr(change, "old"):
                    # A value was removed. If it's observable, stop listening
                    # for changes.
                    if isinstance(change.old, Observable):
                        change.old.unobserve(self._recursive_listener)
                if hasattr(change, "new"):
                    # A value was added. If it's observable, start listening
                    # for changes.
                    if isinstance(change.new, Observable):
                        change.new.observe(self._recursive_listener, True)
        # Now we need to notify all of our own observers about changes.
        for registration in self._observers.itervalues():
            registration["observer"](path, hierarchy, changes, registration["context"])








































