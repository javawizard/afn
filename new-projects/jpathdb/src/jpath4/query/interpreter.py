
from jpath4.query.binders import jpath_binder, native

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
































