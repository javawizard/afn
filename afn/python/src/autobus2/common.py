
from abc import ABCMeta as ABC, abstractmethod as abstract
import autobus2
import inspect

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
    """
    This class isn't yet complete. I'm hoping to use it to merge the stuff in
    Connection, SingleServiceProxy, and MultipleServiceProxy into this class at
    some point in the future.
    """
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


def get_function_doc(object, function_name, remove_self=True):
    function = getattr(object, function_name)
    is_method_before = inspect.ismethod(function)
    while hasattr(function, "wrapped"):
        function = function.wrapped
    if is_method_before or inspect.ismethod(function):
        argspec = inspect.getargspec(function)
        # Remove the leading "self" argument
        if remove_self:
            argspec = (argspec[0][1:],) + argspec[1:]
        args = inspect.formatargspec(*argspec)
    elif inspect.isfunction(function):
        args = inspect.formatargspec(*inspect.getargspec(function))
    else:
        args = "(...)"
    doc = inspect.getdoc(function)
    if doc is None:
        doc = ""
    return function_name + args + "\n\n" + doc




