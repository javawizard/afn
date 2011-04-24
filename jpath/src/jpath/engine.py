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
import jpath.evaluators


# Interpreter manages lots of queries possibly running at the same time,
# QueryState is for a single evaluation of a query, Context is for a
# particular dynamic context in a query. So:
# One Interpreter for several query evaluations
# One QueryState for one query evaluation
# Several Contexts for one query evaluation
# Query instance created from asking an interpreter to parse a query
# Contexts and QueryStates must NOT be tied to a query since they can jump
# queries if one query calls into a function defined by another. Predicate
# instances are, however, tied to a query since they represent how that
# query's code is stored in memory.
# All of these, however, are tied to a particular interpreter.
"""
Hmm... How do we want to go about doing modules? I'm thinking an import would
look something like this:

import module "/some/file.jq" as something;

Functions in the specified module could then be called like this:

something.some-function(arg1, arg2, ...)

Shouldn't be too hard. And, of course, whatever's using the engine would be
able to define behavior for importing modules and, in particular, resolving
variables and functions (the engine can tell if a variable or function is
external by the presence of a "." in its name).

Since the behavior of the string after "import module" is defined by
whatever's using the JPath query engine, it can provide custom modules such as
"/cool-database-system/builtins" or something. I would expect (and I may
define this to be standard behavior for systems running from a folder of
modules on disk) the string to take the form of a typical directory path, with
a certain database-defined folder functioning as the root. The database would
then be responsible for making sure that imports can't escape from that root.

The engine, then, is responsible for loading queries. I'm thinking there
should be two new classes: Module and Function. Module is a class that has
methods for getting the value (as a data.Item) of a particular variable
defined in the module and for getting a Function object representing a
particular function defined within the module. Variable values must not change
during evaluation. Functions provide a way to invoke the function with a
certain set of arguments.

TODO: figure out how the current state of global variables factors into
evaluating the query. Where should this be stored etc? Perhaps have the
context represent what file we're running or something.
"""


class Query(object):
    """
    A representation of a single parsed query or module.
    """
    def __init__(self, interpreter, text, production):
        self.interpreter = interpreter
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
            return context.evaluate(self.production)
        except errors.EvaluationError as e:
            e.set_query(self)
            raise
    
    query = evaluate


class Context(object):
    def __init__(self, query_state):
        self.query_state = query_state
        self.interpreter = query_state.interpreter
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
        return "<Context ...>"
    
    def evaluate(self, production):
        return self.interpreter._evaluate(self, production)
    
    def get_evaluator(self, production_class):
        return self.interpreter.get_evaluator(production_class)


class QueryState(object):
    """
    This class represents the state of a single evaluation of a particular
    query. QueryState objects have a set of options that can be used for any
    particular purpose. For example, JPath Database has a function for setting
    a particular query that should be evaluated after the current query; this
    function simply sets an option on the query state, which JPath Database
    then reads to determine what to do after evaluating the query.
    
    When a QueryState is constructed, a Context instance is also constructed
    for it. This instance is available via the context field of the QueryState
    object. This context, or contexts derived from it, should be passed to
    Query.evaluate when running a query under this QueryState.
    """
    def __init__(self, interpreter):
        """
        FOR INTERNAL USE ONLY. QueryState instances should be created by
        external code ONLY by calling Interpreter.new_state.
        """
        self.interpreter = interpreter
        self.context = Context(self)
        self.options = {}
    
    def set_option(self, name, value):
        """
        Sets an option on this QueryState object. If the value is None, the
        specified option will be removed.
        """
        if value is None:
            del self.options[name]
        else:
            self.options[name] = value
    
    def get_option(self, name, value, default=None):
        """
        Gets the value of the option with the specified name. If there is no
        such option, the specified default value will be returned instead.
        """
        return self.options.get(name, value, default)


class Interpreter(object):
    """
    This class is the main entry point to the JPath query engine.
    """
    def __init__(self, use_default_evaluators=True):
        self.evaluators = {}
        if use_default_evaluators:
            self.install_default_evaluators()
    
    def set_evaluator(self, production_class, evaluator):
        self.evaluators[production_class] = evaluator
    
    def get_evaluator(self, production_class):
        try:
            return self.evaluators[production_class]
        except KeyError:
            raise Exception("No evaluator could be found for production class " 
                    + str(type(production_class)) + ", and specifically the instance "
                    + trimTo(200, repr(production_class)) + ". This means that an "
                    "evaluator for this production class wasn't installed "
                    "using set_evaluator ")
    
    def install_default_evaluators(self):
        for production_class, evaluator in jpath.evaluators.evaluators.items():
            self.set_evaluator(production_class, evaluator)
    
    def parse(self, text):
        results = list(jpath.syntax.pQuery.parseString(text, parseAll=True))
        if len(results) != 1:
            raise Exception("Problem while parsing results: precisely one "
                    "result was expected, but " + str(len(results)) + " were "
                    "provided by the parser. " + JPATH_INTERNAL_ERROR_MESSAGE)
        return Query(self, text, results[0])
    
    def new_state(self):
        return QueryState(self)
    
    def _evaluate(self, context, query):
        """
        DON'T USE THIS EXTERNALLY UNLESS YOU KNOW WHAT YOU'RE DOING. It's for
        JPath internal use. I'll get a tutorial up soon on how to properly run a
        query.
        """
        # Figure out the method to dispatch to based on the type of AST node that
        # this is
        function = self.get_evaluator(type(query))
        try:
            if function is None:
                raise Exception("Evaluate not implemented for AST component type " + 
                        str(type(query)) + " containing " + trimTo(200, repr(query)) + ". "
                        + JPATH_INTERNAL_ERROR_MESSAGE)
            result = function(context, query)
            if not isinstance(result, list):
                raise Exception("Result was not a list (representing a collection) "
                        "for AST component type " + str(type(query)) + " containing " + 
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


 







































