
from jpath4.query.binders import jpath_binder, native
from jpath4.query import utils, translate, context

class Interpreter(object):
    def __init__(self):
        self.binders = {}
        add_default_binders(self)
    
    def add_binder(self, binder, name=None):
        if name is None:
            name = binder.default_name
        self.binders[name] = binder
        binder.set_interpreter(self)
    
    def get_binder(self, name):
        return self.binders[name]
    
    def bind_module(self, type, name):
        if type not in self.binders:
            raise Exception('No binder ' + type +
                    " installed on this interpreter")
        return self.binders[type].bind_module(name)


def add_default_binders(interpreter):
    interpreter.add_binder(jpath_binder.JPathBinder())
    interpreter.add_binder(native.NativeBinder())


def query(jpath_query, json={}, vars={}, interpreter=None, as_tuple=False):
    if interpreter is None:
        interpreter = Interpreter()
    module = interpreter.get_binder("jpath").create_query(jpath_query)
    wrapped_vars = utils.singleton(translate.json_to_jpath(vars))
    result = module.call_with_values(
            context.DynamicContext(translate.json_to_jpath(json), 1, 1), 
            [wrapped_vars])
    if len(result) == 0 and not as_tuple:
        return None
    elif len(result) == 1 and not as_tuple:
        return translate.jpath_to_json(utils.get_single(result))
    else:
        return tuple(translate.jpath_to_json(v) for v in result)

































