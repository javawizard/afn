
from jpath4.query import translate, data as d, utils
from jpath4.query.context import DynamicContext

class Database(object):
    def __init__(self, storage, interpreter_constructor):
        self.storage = storage
        self.interpreter_constructor = interpreter_constructor
    
    def call(self, module_name, function_name, *args):
        """
        Calls the named function in the named module with the specified JSON
        arguments. The JSON result is then returned.
        """
        interpreter = self.interpreter_constructor()
        module = interpreter.bind_module("jpath", module_name)
        function = module.get_function(function_name)
        wrapped_args = [translate.json_to_jpath(v) for v in args]
        result = function.call_with_values(self.make_context(), wrapped_args)
        return translate.jpath_to_json(result)
    
    def run_module(self, module, update, vars={}):
        """
        Runs the named module as a main module. The specified dictionary of
        variables are set just before calling the module; they can be accessed
        as $foo, $bar, etc., for vars={"foo": ..., "bar": ...}.
        
        If update is True, the module's result is assumed to be a sequence of
        zero or more updates, which are applied to the database. If update is
        False, the result of the module, whether or not it's a sequence of
        updates, is returned as-is. If update is None, it will be treated as if
        it were true if the result of the module is a sequence of updates and
        false otherwise.
        """
        interpreter = self.interpreter_constructor()
        module = interpreter.bind_module("jpath", module)
        wrapped_vars = utils.singleton(translate.json_to_jpath(vars))
        result = module.call_with_values(self.make_context(), [wrapped_vars])
        result = self.process_result(result, update)
        return result
        
    def run_query(self, query, update, vars={}):
        """
        Runs the specified query string. The semantics of this method are the
        same as run_module, except that the query text is specified as a
        parameter to this method.
        """
        interpreter = self.interpreter_constructor()
        module = interpreter.get_binder("jpath").create_query(query)
        wrapped_vars = utils.singleton(translate.json_to_jpath(vars))
        result = module.call_with_values(self.make_context(), [wrapped_vars])
        result = self.process_result(result, update)
        return result
    
    def process_result(self, result, update):
        if update is None:
            update = True
            for value in result: # result is a Sequence
                if not isinstance(result, d.Update):
                    update = False
                    break
        if update:
            self.storage.apply_updates(result)
            result = None
        else:
            if len(result) == 0: # Result is the empty sequence
                result = None
            else: # Try to get the single resulting item
                result = translate.jpath_to_json(utils.get_single(result))
        self.storage.commit()
        return result
        
    def make_context(self):
        root = self.storage.get_root()
        return DynamicContext(root, 1, 1).new(userland={"db.get_root": lambda: root})
    
