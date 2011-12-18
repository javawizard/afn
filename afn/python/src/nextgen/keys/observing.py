
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
            def _recursive_listener_function(source, changes):
                for registration in self._observers.values():
                    if registration["recursive"]:
                        registration["observer"](self, changes)
            self._recursive_listener = _recursive_listener_function
    
    def observe(self, observer, recursive=False):
        self._init_observable()
        if id(observer) in self._observers:
            raise ValueError("The specified observer has already been added to "
                             "this object.")
        registration = {"observer": observer, "recursive": recursive}
        self._observers[id(observer)] = registration
        if recursive:
            self._recursive_observer_count += 1
            if self._recursive_observer_count == 1: # This was the first
                # recursive observer to be added
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
        del self._observers[id(observer)]
        if registration["recursive"]:
            self._recursive_observer_count -= 1
            if self._recursive_observer_count == 0: # Last recursive observer
                # to be removed
                values = []
                self._get_current_values(values)
                for value in values:
                    if isinstance(value, Observable):
                        value.unobserve(self._recursive_listener)
    
    def _notify_changed(self, changes):
        if len(changes) == 0:
            return
        self._init_observable()
        if self._recursive_observer_count > 0:
            for change in changes:
                if hasattr(change, "old"):
                    if isinstance(change.old, Observable):
                        change.old.unobserve(self._recursive_listener)
                if hasattr(change, "new"):
                    if isinstance(change.new, Observable):
                        change.new.observe(self._recursive_listener, True)
        for registration in self._observers.itervalues():
            registration["observer"](self, changes)








































