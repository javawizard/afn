# coding=UTF-8

from pyparsing import ParseException
import pyparsing
import itertools
import sys
from copy import copy as shallow_copy
from traceback import print_exc
# Importing ourselves; we end up using this to look up which function to call
# for each component of the AST
import jpath.engine #@UnresolvedImport
import jpath.syntax
from jpath.data import Boolean, Item, List, Map, Null, Number, Pair, String
import jpath.errors as errors
from jpath.utils.messages import JPATH_INTERNAL_ERROR_MESSAGE
from jpath.utils.text import trimTo



class Query(object):
    def __init__(self, text, production):
        self.text = text
        self.production = production
        self.set_production_query(self.production)
    
    def set_production_query(self, production):
        if isinstance(production, jpath.syntax.Production):
            production.p_query = self
            for var in production.p_varnames:
                self.set_production_query(getattr(production, var))
        elif isinstance(production, (list, tuple)):
            for item in production:
                self.set_production_query(item)
    
    def evaluate(self, context):
        """
        Runs this query in the specified context and returns the result.
        """
        try:
            return evaluate(context, self.production)
        except errors.EvaluationError as e:
            e.set_query(self)
            raise
    
    query = evaluate


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
        if not isinstance(new_item, Item):
            raise Exception("The context item must be an instance of a "
                    "subclass of Item, not an instance of " + 
                    str(type(new_item)))
        context = self.copy()
        context.item = new_item
        return context
    
    def __repr__(self):
        return "Context(" + repr(self.item) + ", " + repr(self.vars) + ")"


class Interpreter(object):
    def __init__(self):
        pass
    
    def parse(self, text):
        results = list(jpath.syntax.pExpr.parseString(text, parseAll=True))
        if len(results) != 1:
            raise Exception("Problem while parsing results: precisely one "
                    "result was expected, but " + str(len(results)) + " were "
                    "provided by the parser. " + JPATH_INTERNAL_ERROR_MESSAGE)
        return Query(text, results[0])
    
    def evaluate(self, context, query):
        """
        DON'T USE THIS EXTERNALLY UNLESS YOU KNOW WHAT YOU'RE DOING. It's for
        JPath internal use. I'll get a tutorial up soon on how to properly run a
        query.
        """
        # Figure out the method to dispatch to based on the type of AST node that
        # this is
        typename = type(query).__name__
        function = getattr(jpath.engine, "evaluate_" + typename, None) # Get the
        # function on this module that's supposed to process this AST component
        try:
            if function is None:
                raise Exception("Evaluate not implemented for AST component type " + 
                        typename + " containing " + trimTo(200, repr(query)) + ". "
                        + JPATH_INTERNAL_ERROR_MESSAGE)
            result = function(context, query)
            if not isinstance(result, list):
                raise Exception("Result was not a list (representing a collection) "
                        "for AST component type " + typename + " containing " + 
                        trimTo(200, repr(query)) + ". " + JPATH_INTERNAL_ERROR_MESSAGE)
            return result
        except errors.EvaluationError as e:
            # TODO: in the future, add additional information to this as we travel
            # up the stack
            raise
        except Exception as e:
            if context.get_option("jpath.python_traceback", False):
                print_exc()
            raise errors.EvaluationError(query.parse_location, type(e).__name__ + ": " + str(e), sys.exc_info())


 







































