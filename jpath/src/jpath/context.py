
import jpath.data as data
from copy import copy as shallow_copy

class Context(object):
    def __init__(self):
        self.item = None
        self.vars = {}
        self.options = {}
    
    def copy(self):
        return shallow_copy(self)
    
    def get_var(self, name):
        """
        same as self.var(name)
        """
        return self.var(name)
    
    def var(self, name):
        return self.vars[name]
    
    def get_option(self, name, default=None):
        return self.options.get(name, default)
    
    def get_options(self):
        return self.options
    
    def new_with_vars(self, vars):
        for key, value in vars.items():
            if not isinstance(key, basestring):
                raise Exception("Variable names have to be strings, not "
                        "values of type " + str(type(key)))
            if not isinstance(value, list):
                raise Exception("Variable values have to be collections "
                        "(represented in Python as lists). So, instead of, for "
                        "example, c.new_with_var('foo', Number(1)), do "
                        "c.new_with_var('foo', [Number(1)]). Specifically, "
                        "you just tried to assign a value of type " + 
                        str(type(value)) + " to the var " + str(key) + ".")
        new_vars = dict(self.vars)
        new_vars.update(vars)
        context = self.copy()
        context.vars = new_vars
        return context
    
    def new_with_var(self, name, value):
        return self.new_with_vars({name: value})
    
    def new_with_options(self, options):
        new_options = dict(self.options)
        new_options.update(options)
        context = self.copy()
        context.options = new_options
        return context
    
    def new_with_option(self, name, value):
        return self.new_with_options(name, value)
    
    def new_with_item(self, new_item):
        if not isinstance(new_item, data.Item):
            raise Exception("The context item must be an instance of a "
                    "subclass of Item, not an instance of " + 
                    str(type(new_item)))
        context = self.copy()
        context.item = new_item
        return context
    
    def __repr__(self):
        return "Context(" + repr(self.item) + ", " + repr(self.vars) + ")"

