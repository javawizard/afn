
from abc import ABCMeta as ABC, abstractmethod as abstract
import autobus2

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


class ServiceConnector(object):
    __metaclass__ = ABC
    
    @abstract
    def _call_function(self, args, callback, timeout, safe):
        pass
    
    def __getitem__(self, name):
        return self.functions[name]
    
    def _sc_get_function(self, name):
        return ServiceConnectorFunction(self, name)
    
    @property
    def functions(self):
        # TODO: cache this
        return ServiceConnectorAccessor(self._sc_get_function)


class ServiceConnectorAccessor(object):
    def __init__(self, accessor):
        self.accessor = accessor
    
    def __getattr__(self, name):
        return self.accessor(name)
    
    def __getitem__(self, name):
        return self.accessor(name)


class ServiceConnectorFunction(object):
    def __init__(self, connector, name):
        self.connector = connector
        self.name = name
    
    def __call__(self, *args, **kwargs):
        callback = kwargs.get("callback", autobus2.SYNC)
        timeout = kwargs.get("timeout", 30)
        safe = kwargs.get("safe", False)
        return self.connector._call_function(args, callback, timeout, safe)



