
from jpath4.query import translate, data as d, utils
from jpath4.query.context import DynamicContext

class Database(connection):
    def __init__(self, datastore, interpreter_constructor):
        self.datastore = datastore
        self.interpreter_constructor = interpreter_constructor
    
    def call(self, module_name, function_name, *args):
        interpreter = self.interpreter_constructor()
        module = interpreter.bind_module("jpath", module_name)
        function = module.get_function(function_name)
        wrapped_args = [translate.json_to_jpath(v) for v in args]
        result = function.call_with_values(self.make_context(), wrapped_args)
        return translate.jpath_to_json(result)
    
    def run_module(self, module, update, vars={}):
        interpreter = self.interpreter_constructor()
        module = interpreter.bind_module("jpath", module)
        wrapped_vars = utils.singleton(translate.json_to_jpath(vars))
        result = module.call_with_values(self.make_context(), [wrapped_vars])
        result = self.process_result(result, update)
        return result
        
    def run_query(self, query, update, vars={}):
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
            self.datastore.apply_updates(result)
            result = None
        else:
            result = translate.jpath_to_json(result)
        self.datastore.commit()
        return result
        
    def make_context(self):
        root = self.datastore.get_root()
        return DynamicContext(root, 1, 1).new(userland={"db.get_root": lambda: root})
    
