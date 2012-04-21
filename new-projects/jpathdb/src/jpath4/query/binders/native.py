
from jpath4.query import binder, module, data, exceptions, utils

class NativeBinder(binder.Binder):
    default_name = "native"
    
    def __init__(self):
        self.module_cache = {}
    
    def bind_module(self, name):
        if name in self.module_cache:
            return self.module_cache[name]
        module = NativeModule(name)
        self.module_cache[name] = module
        return module


class NativeModule(module.Module):
    def __init__(self, name):
        self.name = name
        self.last_name = name.rsplit(".", 1)[-1]
        self.py_module = __import__(name, {}, {}, [self.last_name])
    
    def get_default_bind_name(self):
        return self.last_name
    
    def get_function(self, name):
        function = getattr(self.py_module, name, None)
        if function is None:
            function = getattr(self.py_module, name + "_", None)
        if function is None:
            function = getattr(self.py_module, "_functions", {}
                    ).get(name, None)
        if function is None:
            raise exceptions.FunctionLookupException("No such function " + name + " on native module " + self.name)
        # TODO: have this cache NativeFunction objects, which should make it
        # run a bit faster
        return NativeFunction(self, name, function)


class NativeFunction(module.Function):
    def __init__(self, module, name, py_function):
        self.module = module
        self.name = name
        self.py_function = py_function
    
    def get_min_args(self):
        return 0
    
    def get_max_args(self):
        return 1024 # arbitrary, but I can't think of a sane function that
        # would actually take more arguments than this
    
    def get_closures(self, arg_count):
        # TODO: perhaps have some sort of variable on functions that can be
        # set with an annotation provided for native modules to use that
        # specifies which arguments are closures
        return [False] * arg_count
    
    def call_function(self, dynamic, args):
        # TODO: see above comment in get_closures; support for closures would
        # need to be added here too. Closures should be Python callable
        # objects, probably where keyword arguments can specify named
        # variables or something. **some_dict allows keys to have chars that
        # aren't normally allowed in Python variable identifiers, so this
        # would feasibly work as a mechanism for passing variable values in.
        result = self.py_function(dynamic, *args)
        if result is None: # If the function doesn't return anything, create
            # an empty sequence as its result
            result = utils.create_empty()
        # Make sure the result is a sequence
        if not isinstance(result, data.Sequence):
            raise Exception("Native function " + self.name + " in module "
                    + self.module.name + " didn't return a Sequence object")
        return result





























