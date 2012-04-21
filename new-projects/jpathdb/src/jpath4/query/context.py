
from copy import copy
from jpath4.query import exceptions as e

class Context(object):
    def new(self, **kwargs):
        new = copy(self)
        for thing in self._things:
            if thing in kwargs:
                setattr(new, thing, kwargs[thing])
        return new


class StaticContext(Context):
    _things = ["interpreter", "module", "function"]
    
    def __init__(self, interpreter, module, function):
        self.interpreter = interpreter
        self.module = module
        self.function = function
    
    def find_function(self, name):
        # TODO: consider splitting the name into two components, module and
        # name. These could then be parsed apart by the syntax module itself,
        # which would allow backslashed dots to appear in the module name.
        module_name, _, function_name = name.partition(".")
        if function_name == "" and module_name != "":
            function_name, module_name = module_name, function_name
        if module_name == "":
            # Empty module, so start search with the local function namespace
            function = self.module.functions.get(function_name)
            if function: # Found the function in the local namespace
                return function
            # Didn't find it, so try imported modules as functions
            function = self.module.imports.get(function_name)
            if function: # Found it as a module, and modules are functions, so
                # return it.
                return function
            # Didn't find it, so check imported functions now.
            function = self.module.imported_functions.get(function_name)
            if function: # Found it in imported functions
                return function
            # Didn't find it, so try the prelude now
            function = self.module.prelude.get_function(function_name)
            if function: # Found it in the prelude
                return function
            # Didn't find it, so we continue and raise an exception.
        else:
            # Named module, so check in the list of imported modules.
            module = self.module.imports.get(module_name)
            if module:
                # Found a module, so try to get the specified function from it
                function = module.get_function(function_name)
                if function: # Found the function
                    return function
        # Didn't find anything, so toss back an exception
        raise e.FunctionLookupException("Couldn't find function %s in module "
                "%s, being called from module %s function %s" % (
                        function_name, module_name, self.function.name,
                        self.module.name))


class DynamicContext(Context):
    _things = "context_item", "context_position", "context_size"
    
    def __init__(self, context_item, context_position, context_size):
        self.context_item = context_item
        self.context_position = context_position
        self.context_size = context_size
        self.userland = {}
    
    def new(self, **kwargs):
        new = Context.new(self, **kwargs)
        if "userland" in kwargs:
            new.userland.update(kwargs["userland"])
        if "unset_userland" in kwargs:
            del new.userland[kwargs["unset_userland"]]
        return new


class LocalContext(Context):
    _things = []
    
    def __init__(self):
        self.vars = {}
    
    def new(self, **kwargs):
        """
        Allows either set_name="...",set_value="..." or set_map={...} or
        unset_name="..."
        """
        new = Context.new(self, **kwargs)
        if "set_name" in kwargs:
            new.vars[kwargs["set_name"]] = kwargs["set_value"]
        if "set_map" in kwargs:
            new.vars.update(kwargs["set_map"])
        if "unset_name" in kwargs:
            del new.vars[kwargs["unset_name"]]
        return new
    
    def get_var(self, name):
        result = self.vars.get(name, None)
        if result is None:
            raise Exception("No such variable: " + name)
        return result


