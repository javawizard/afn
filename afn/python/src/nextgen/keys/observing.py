

class Observable(object):
    def _get_current_values(self, value_list):
        pass
    
    def _recursive_listener(self, source, changes):
        pass
    
    def _init_observable(self):
        try:
            self._observers
        except AttributeError:
            self._observers = {}
    
    def observe(self, listener, recursive=False):
        self._init_observable()
        