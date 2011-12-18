
from afn.utils import singleton

# _NO_VALUE = singleton.Singleton("nextgen.keys.observing._NO_VALUE")

class Observable(object):
    def _get_current_values(self, value_list):
        pass
    
    def _recursive_listener(self, source, changes):
        for observer, registration in self._observers.items():
            if registration["recursive"]:
                observer(self, changes)
    
    def _init_observable(self):
        try:
            self._observers
        except AttributeError:
            self._observers = {}
            self._recursive_observer_count = 0
    
    def observe(self, observer, recursive=False):
        self._init_observable()
        if observer in self._observers:
            raise ValueError("The specified observer has already been added to "
                             "this object.")
        registration = {"recursive": recursive}
        self._observers[observer] = registration
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
        if observer not in self._observers:
            raise ValueError("The specified observer has not been added as to "
                             "this object.")
        registration = self._observers[observer]
        del self._observers[observer]
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
        if self._recursive_observer_count > 0:
            for change in changes:
                if hasattr(change, "old"):
                    if isinstance(change.old, Observable):
                        change.old.unobserve(self._recursive_listener)
                if hasattr(change, "new"):
                    if isinstance(change.new, Observable):
                        change.new.observe(self._recursive_listener, True)
        for observer in self._observers:
            observer(self, changes)








































