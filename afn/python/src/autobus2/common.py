
class AutoClose(object):
    """
    A class intended to be used as a superclass by classes with a close()
    function. AutoClose turns classes that subclass it into context managers
    whose __exit__ functions cause self.close() to be run. The __enter__ and
    __exit__ provided by AutoClose are reentrant.
    
    AutoClose's __enter__ returns self.
    """
    def __enter__(self):
        try:
            self._autoclose_enters += 1
        except AttributeError:
            self._autoclose_enters = 1
        return self
    
    def __exit__(self, *args):
        self._autoclose_enters -= 1
        if self._autoclose_enters == 0:
            self.close()