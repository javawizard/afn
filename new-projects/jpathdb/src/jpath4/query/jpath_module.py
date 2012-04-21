
from jpath4.query import module, productions, context, constants, utils

class JPathModule(module.Module):
    def __init__(self, interpreter, path):
        self.imports = {}
        self.options = {}
        self.functions = {}
        self.imported_functions = {}
        self.interpreter = interpreter
        self.path = path
        self.main_expr = None
        self.main_function = None
        _, _, self.name = path.rpartition("/")
        self.name, _, _ = self.name.partition(".")
    
    def get_default_bind_name(self):
        return self.name
    
    def get_function(self, name):
        return self.functions.get(name)
    
    def load(self, production):
        """
        Initializes this module from an instance of
        jpath.query.productions.Module.
        """
        # First thing we're going to do is bind the prelude module and import
        # all of its functions.
        prelude = self.interpreter.bind_module(*constants.prelude)
        self.prelude = prelude
        # Prelude has been imported. Now we resolve module-declared imports.
        for import_statement in production.prolog:
            if not isinstance(import_statement, productions.Import):
                continue
            binder = import_statement.binder
            source = import_statement.source
            target = import_statement.target
            if binder is None:
                binder = "jpath" # TODO: put this in some global field so
                # that it's not hard-coded here
            module = self.interpreter.bind_module(binder, source)
            if target is None:
                target = module.get_default_bind_name()
            self.imports[target] = module
        # Imports have been resolved. Now load options.
        for option in production.prolog:
            if not isinstance(option, productions.Option):
                continue
            self.options[option.name] = option.value
        # Now we load all the functions.
        for function in production.functions:
            args = []
            closures = []
            defaults = []
            name = function.name
            expr = function.expr
            for arg in function.args:
                args.append(arg.name)
                closures.append(arg.type == "closure")
                has_default = arg.default is not None
                if not has_default and len(defaults) > 0:
                    raise Exception('function "' + name + '" has an arg '
                            "without a default after an arg with a default")
                if has_default:
                    defaults.append(arg.default)
            self.functions[name] = JPathFunction(self, name, args, closures,
                    defaults, expr)
        # Functions have been loaded. Last thing to do is load the main
        # expression, and we're done!
        self.main_expr = production.expr
        self.main_function = JPathFunction(self, "<main>", [], [], [], self.main_expr)
    
    def get_min_args(self):
        return 0
    
    def get_max_args(self):
        return 1
    
    def get_closures(self, arg_count):
        return [False] * arg_count
    
    def call_function(self, dynamic_context, args):
        """
        Calls this module as a function, which runs the module's main code.
        
        Args is the empty list to call a module, I.E. modules don't require
        any JPath function arguments. A single, optional argument can,
        however, be provided in this list; it should be a JPath sequence
        containing exactly one value, a JPath object. The keys of this object
        will be used as the names of variables to predefine when running this
        module, and the values will be used as the values of the variables.
        (For now, this means that all variable values will end up being
        singleton sequences; I might add some way around this in the future.) 
        """
        if len(args) == 0:
            local_context = None
        else:
            sequence = args[0]
            dictionary = sequence.get_item(0)
            var_map = {}
            for pair in dictionary:
                key, value = pair.get_key().get_value(), pair.get_value()
                if not isinstance(key, (str, unicode)):
                    raise TypeError(type(key))
                var_map[key] = utils.singleton(value)
            local_context = context.LocalContext().new(set_map=var_map)
        return self.main_function.call_function(dynamic_context, [], local_context)
    
    def __repr__(self):
        return "<JPathModule %s at %s>" % (repr(self.name), repr(self.path))


class JPathFunction(module.Function):
    def __init__(self, module, name, args, closures, defaults, expr):
        """
        module -> containing module
        name -> function name
        args -> list of strings, argument names
        closures -> list of booleans, whether or not each arg is a closure
        defaults -> list of defaults, smaller than or same size as args,
        aligned to end of args
        expr -> actual contents of the function
        """
        self.module = module
        self.name = name
        self.args = args
        self.closures = closures
        self.defaults = defaults
        self.expr = expr
        self.min_args = len(args) - len(defaults)
        self.max_args = len(args)
        self.padded_defaults = [None] * self.min_args + defaults
    
    def get_min_args(self):
        return self.min_args
    
    def get_max_args(self):
        return self.max_args
    
    def get_closures(self, arg_count):
        return self.closures[:arg_count]
    
    def call_function(self, dynamic_context, args, local_context=None):
        # TODO: document this and note that local_context should almost always
        # be None, but is present to allow specialized applications to inject
        # variables into the function
        if len(args) < self.min_args or len(args) > self.max_args:
            raise Exception("Invalid number of arguments (%s) to function "
                    "%s in module %s" % (len(args), self.name, self.module.path))
        # TODO: Figure out a way to evaluate defaults only once to save
        # computation time
        static_context = context.StaticContext(self.module.interpreter, self.module, self)
        if local_context is None:
            local_context = context.LocalContext()
        default_values = []
        for i in range(len(args), self.max_args):
            default_values.append(self.defaults[i].evaluate(static_context, dynamic_context, local_context))
        # Now set the arguments into variables
        args_with_defaults = args + default_values
        arg_vars = {}
        for name, value in zip(self.args, args_with_defaults):
            arg_vars[name] = value
        local_context = local_context.new(set_map=arg_vars)
        # Now we run the function, and we're done!
        result = self.expr.evaluate(static_context, dynamic_context, local_context)
        return result
    
    def __repr__(self):
        return "<JPathFunction %s in %s>" % (repr(self.name), repr(self.module))

















































