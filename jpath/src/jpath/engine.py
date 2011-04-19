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


def trimTo(length, text):
    if len(text) < length:
        return text
    return text[:length] + "..."


JPATH_INTERNAL_ERROR_MESSAGE = ("This is a JPath internal error; report this "
        "to the JPath developers and they'll get it fixed as soon as "
        "possible. (one of them is alex@opengroove.org in case you don't "
        "have any other information about how to get in touch with them.)")


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
    
    def evaluate(self, context=Context()):
        """
        Runs this query in the specified context and returns the result.
        """
        try:
            return evaluate(context, self.production)
        except EvaluationError as e:
            e.set_query(self)
            raise
    
    query = evaluate


class EvaluationError(Exception):
    """
    Exception thrown when an error occurs while running a query.
    """
    def __init__(self, location, message, cause_info):
        self.location = location
        self.message = message
        self.cause_info = cause_info
        self.query = None
    
    def set_query(self, query):
        self.query = query
    
    def generate_location(self):
        if self.query:
            return ("line " + str(pyparsing.lineno(self.location, self.query.text)) +
                    ", col " + str(pyparsing.col(self.location, self.query.text)))
        else:
            return "position " + str(self.location)
    
    def __str__(self):
        return "Error at " + self.generate_location() + ": " + self.message
    
    def __repr__(self):
        return "<EvaluationError at " + self.generate_location() + ": " + self.message + ">"


def python_to_jpath(data):
    """
    Converts a Python representation of JSON data (the data that a call to
    simplejson.loads would return) into the JSON data representation used by
    the JPath query engine internally. You'll need to call this on any values
    that you want to set into variables (or even the context item) when
    evaluating a query.
    
    The return value will be a subclass of Item.
    """
    if isinstance(data, basestring):
        return String(data)
    elif isinstance(data, bool):
        return Boolean(data)
    elif isinstance(data, (int, long, float)):
        return Number(float(data))
    elif data is None:
        return Null()
    elif isinstance(data, dict):
        return Map([Pair(python_to_jpath(k), python_to_jpath(v)) for k, v in data.items()])
    elif isinstance(data, (tuple, list)):
        return List([python_to_jpath(v) for v in data])
    else:
        raise Exception("No python -> jpath encoding for " + str(type(data)))


def jpath_to_python(data):
    """
    The opposite of python_to_jpath: converts a representation used internally
    into JSON data suitable for, as an example, passing to simplejson.dumps.
    """
    if isinstance(data, (String, Boolean)):
        return data.get_value()
    elif isinstance(data, Number):
        if data.is_whole():
            return data.get_integer()
        else:
            return data.get_float()
    elif isinstance(data, Null):
        return None
    elif isinstance(data, List):
        return [jpath_to_python(v) for v in data.get_items()]
    elif isinstance(data, Map):
        return dict([(jpath_to_python(p.get_key()), jpath_to_python(p.get_value())) for p in data.get_pairs()])
    else:
        raise Exception("No jpath -> python encoding for " + str(type(data)))


def parse(text):
    results = list(jpath.syntax.pExpr.parseString(text, parseAll=True))
    if len(results) != 1:
        raise Exception("Problem while parsing results: precisely one "
                "result was expected, but " + str(len(results)) + " were "
                "provided by the parser. " + JPATH_INTERNAL_ERROR_MESSAGE)
    return Query(text, results[0])



def is_true_value(value):
    if isinstance(value, Null):
        return False;
    elif isinstance(value, Boolean):
        return value.get_value()
    return True;


def is_true_collection(collection):
    if len(collection) == 0:
        return False
    return all(map(is_true_value, collection)) # A collection is true if it's
    # not empty and every one of its values is also true


def extract_single(collection):
    if len(collection) != 1:
        raise Exception("Expected exactly one item but received a collection "
                "of " + str(len(collection)) + " items")
    return collection[0]


def as_type(value, type):
    """
    Returns value if it's an instance of type. If it isn't, an exception will
    be thrown.
    """
    if not isinstance(value, type):
        raise Exception("Expected value of type " + str(type) + " but a "
                "value of type " + str(type(value)) + " was received instead")
    return value


def binary_comparison(context, left_expr, right_expr, function):
    """
    Performs a binary comparison between the values of the two expressions.
    These expressions are each evaluated. Each item in the two resulting
    collections is compared with each item in the other of the two collections
    by calling function, which should be of the form lambda x, y: ... where x
    is a value resulting from the evaluation of left_expr and y is a value
    resulting from the evaluation of right_expr. function should return True
    or False. If it returns True for any pairs of items, this function will
    return Boolean(True). Otherwise, this function will return Boolean(False).
    """
    left_value = evaluate(context, left_expr)
    right_value = evaluate(context, right_expr)
    return Boolean(any([function(x, y) for x in left_value for y in right_value]))


def binary_operation(context, left_expr, right_expr, function):
    """
    Performs a binary operation. This evaluates left_expr and right_expr under
    the specified context, then makes sure that both of them contain exactly
    one item. If they contain more than one item, an exception is thrown. Then
    function is called, passing in the single left value and the single right
    value, and the result is returned.
    """
    left_value = evaluate(context, left_expr)
    right_value = evaluate(context, right_expr)
    return function(extract_single(left_value), extract_single(right_value))


def arithmetic_operation(context, query, function):
    """
    Performs an arithmetic operation. This evaluates query.left and
    query.right, ensures that they contain only one item, extracts the float
    values from the results, and passes them to function. The return value is
    then wrapped in a Number, and the result placed in a list (representing a
    collection) and returned.
    
    The result of this function is already packaged in a collection; you do
    not need to wrap the result in a list before returning it from an
    evaluate_* function.
    """
    def operation(x, y):
        return Number(function(as_type(x, Number).get_float(), as_type(y, Number).get_float()))
    return [binary_operation(context, query.left, query.right, operation)]


def run_input(context=Context(), loop=True):
    """
    Prompts for a line of text with raw_input(), then evaluates it as a query.
    The results will then be printed to stdout, along with a summary.
    
    If loop is true, the user will be prompted again for another query, and so
    on until they hit enter without entering a query. Otherwise, this function
    will return after processing one query.
    
    The queries will be run under the specified context.
    """
    while True:
        text = raw_input("Q>> ")
        if text == "":
            return
        try:
            results = parse(text).query(context)
            if len(results) == 0:
                print "-- No results."
            elif len(results) == 1:
                print "-- 1 result:"
            else:
                print "-- " + str(len(results)) + " results:"
            for result in results:
                print repr(jpath_to_python(result))
        except ParseException as e:
            print "Error while parsing: " + str(e)
        except EvaluationError as e:
            print "Error while evaluating: " + str(e)
        if not loop:
            return


def run_file(context, filename):
    """
    Runs the specified file in the specified context. This reads the file's
    contents, parses them into a query, and calls the resulting query's query
    method.
    """
    return parse_file(filename).query(context)


def parse_file(filename):
    """
    Reads the contents of the specified file and calls parse, passing in the
    file's text. The result of the call to parse is returned.
    """
    with open(filename) as file:
        return parse(file.read())


def query_against_python_item(query, item=None):
    """
    Converts the specified item, if specified, to JPath's JSON representation
    by passing it into python_to_jpath, then constructs a Context from it and
    calls evaluate with that context and the specified query. The result of
    this call to evaluate is then passed into jpath_to_python, and the result
    is then returned from this method.
    
    Right now, the contents of this function are simply this:

    if item is None:
        context = Context()
    else:
        context = Context(python_to_jpath(item))
    return jpath_to_python(parse(query).query(context))
    """
    if item is None:
        context = Context()
    else:
        context = Context(python_to_jpath(item))
    return jpath_to_python(parse(query).query(context))


def evaluate(context, query):
    """
    TODO: This should not be used externally anymore. Instead, queries should
    be constructed by parsing them with parse, and then calling query on the
    resulting query object.
    
    Runs the specified query in the specified context, which should be a
    Context instance. The context can specify a context item and a set of
    variables that will already be assigned values when the query runs.
    
    The query can be either a string representing the query to run or the
    return value of parse(query_text). If you're going to be running the same
    query over and over again, you should generally call parse and store the
    result to avoid the delay of having to re-parse the query every time. I
    ran some benchmarks on a 1.66GHz Intel Core 2 Duo and storing the results
    of a call to parse and just evaluating that stored value resulted in
    queries running about 300 times faster (300 seconds, or 5 minutes, to run
    the query {{"a": "b", "c": "d"}/@a} 30000 times from text as opposed to
    1 second to run this query pre-parsed 30000 times). 
    
    This function returns a list of all of the items selected by the query.
    
    Here's some examples. These use eval_against_python_item for the sake of
    conciseness; eval_against_python_item is just a wrapper around this
    function (evaluate) that performs some additional conversions; see the
    documentation of eval_against_python_item for more information on what,
    exactly, it does.
    
    >>> eval_against_python_item("foo", {"foo": "bar"})
    ["bar"]
    
    >>> eval_against_python_item("foo/bar", {"foo": {"bar": "baz"}})
    ["baz"]
    
    >>> eval_against_python_item("*/foo", [{"foo": "bar"}, {"foo": "baz"}])
    ["bar", "baz"]
    
    >>> eval_against_python_item('{{"a": "b", "c": "d"}/@a}')
    {"a": "b"}
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
    except EvaluationError as e:
        # TODO: in the future, add additional information to this as we travel
        # up the stack
        raise
    except Exception as e:
        if context.get_option("jpath.python_traceback", False):
            print_exc()
        raise EvaluationError(query.parse_location, type(e).__name__ + ": " + str(e), sys.exc_info())

def evaluate_NumberLiteral(context, query):
    return [Number(query.value)]

def evaluate_StringLiteral(context, query):
    return [String(query.value)]

def evaluate_VarReference(context, query):
    return context.var(query.name)

def evaluate_BooleanLiteral(context, query):
    return [Boolean(query.value)]

def evaluate_NullLiteral(context, query):
    return [Null()]
    
def evaluate_ContextItem(context, query):
    # Context item is a single item, not a collection, so surround it in a
    # list (which represents a collection)
    return [context.item]

def evaluate_Children(context, query):
    # This is the * symbol. Basically, if the context item is a map, then we
    # get all of its values. If it's a list, we get all of its items.
    # Otherwise, we return the empty collection.
    if isinstance(context.item, Map):
        return context.item.get_values()
    elif isinstance(context.item, List):
        return context.item.get_items()
    else:
        return []

def evaluate_PairChildren(context, query):
    # This represents @* in a query. If the context is a map, then we get all
    # of its pairs. Otherwise, we return the empty collection.
    if isinstance(context.item, Map):
        return context.item.get_pairs()
    else:
        return []

def evaluate_Pattern(context, query):
    # If this is a map, we get the value of the specified key if it exists.
    if isinstance(context.item, Map):
        value = context.item.get_value(String(query.value))
        if value:
            return [value]
        else: # No such value, so return the empty collection
            return []
    # If this is a pair, get its key or value if the pattern is "key" or
    # "value", respectively
    elif isinstance(context.item, Pair):
        if query.value == "key":
            return [context.item.get_key()]
        elif query.value == "value":
            return [context.item.get_value()]
    # Not a map or a pair, or if it was a pair the pattern wasn't "key" or
    # "value", so we'll return the empty collection.
    return []

def evaluate_PairPattern(context, query):
    # These only work on maps.
    if isinstance(context.item, Map):
        value = context.item.get_pair(String(query.value))
        if value: # We have a pair
            return [value]
    # Context item wasn't a map or didn't have an entry with the specified key
    return []

def evaluate_ParenExpr(context, query):
    # Just evaluate the expression inside parens and return the result
    return evaluate(context, query.expr)

def evaluate_ListConstructor(context, query):
    return [List(list(evaluate(context, query.expr)))] # Create a list
    # containing the items in the collection resulting from evaluating
    # the specified expression 

def evaluate_EmptyListConstructor(context, query):
    return [List([])] # Return a collection containing a single empty list

def evaluate_MapConstructor(context, query):
    collection = evaluate(context, query.expr) # Evaluate the expression
    # containing the pairs that should go into the new map
    return [Map(collection)]

def evaluate_EmptyMapConstructor(context, query):
    return [Map([])]

def evaluate_EmptyCollectionConstructor(context, query):
    return []

def evaluate_Indexer(context, query):
    # Evaluate the indexer's expression
    value = evaluate(context, query.expr)
    # If the indexer expression resulted in the empty collection, then return
    # the empty collection
    if len(value) < 1:
        return []
    # The expression passed to an indexer should only contain one item. TODO:
    # change this to allow multiple items to select from multiple indexes, and
    # have a x to y operator that returns a collection of numbers from x to y
    value = value[0]
    # Now we actually do useful stuff. Let's see if we're querying a list...
    if(isinstance(context.item, List)):
        # Yup, we are. So, first things first: convert the value, which should
        # be a Number, to an int.
        if not isinstance(value, Number):
            # TODO: consider allowing strings, booleans, etc, and converting
            # them to int values (strings -> ints by parsing them, booleans ->
            # ints by using 0 for false and 1 for true
            raise Exception("The argument to an indexer has to be a number.")
        # It's a number, so we get the int value from it.
        value = value.get_integer()
        # Now we get the items. The return value of get_item_range is a list
        # of all the items requested, so we'll just return that list as the
        # collection of items (which will only have one item in it).
        return context.item.get_item_range(value - 1, value)
    # Not a list. Is it a map?
    elif(isinstance(context.item, Map)):
        # Yup, so let's see if it has an entry with the specified key.
        result = context.item.get_value(value)
        # Does this entry exist?
        if result is not None:
            # Yes it does, so we'll return a collection containing it.
            return [result]
        # It doesn't, so we'll return the empty collection.
        return []
    # Context item isn't a list or a map, so we return the empty collection.
    # TODO: consider adding support for indexing strings, which should return
    # a substring of the original string
    return []

def evaluate_PairIndexer(context, query):
    # Same as evaluate_Indexer, but only works for maps, and gets the pair
    # corresponding to the specified entry instead of the entry's value
    value = evaluate(context, query.expr)
    if len(value) < 1:
        return []
    value = value[0]
    if(isinstance(context.item, Map)):
        result = context.item.get_pair(value)
        if result is not None:
            return [result]
    return []

def evaluate_Path(context, query):
    # Evaluate the left-hand side
    left_value = evaluate(context, query.left)
    # Then, for each value in the left-hand side's resulting collection,
    # create a new context with that value as the context item, run the
    # right-hand side under that context, and put the resulting collection
    # into result_collections
    result_collections = [evaluate(context.new_with_item(v), query.right) for v in left_value]
    # Now we flatten out the list of collections into one list (representing
    # a collection) containing all the items, and return it.
    return list(itertools.chain(*result_collections))

def evaluate_Predicate(context, query):
    # We evaluate the left-hand side
    left_value = evaluate(context, query.left)
    # Then we go through the list of items in the resulting left-hand
    # collection. For each item, we evaluate the predicate with the specified
    # item as the context item. If the result of evaluating the predicate is
    # a true collection (a collection with at least one true value), then we
    # include the item in the list of values to return. 
    return [v for v in left_value if is_true_collection(evaluate(context.new_with_item(v), query.right))]

def evaluate_Multiply(context, query):
    return arithmetic_operation(context, query, lambda x, y: x * y)

def evaluate_Divide(context, query):
    return arithmetic_operation(context, query, lambda x, y: float(x) / y)

def evaluate_Add(context, query):
    return arithmetic_operation(context, query, lambda x, y: x + y)

def evaluate_Subtract(context, query):
    return arithmetic_operation(context, query, lambda x, y: x - y)

def evaluate_Otherwise(context, query):
    # The right-hand side should only be evaluated if the left-hand side is
    # the empty sequence, so we can't use binary_operation for this
    left_expr = query.left
    right_expr = query.right
    left_value = evaluate(context, left_expr)
    if left_value == []:
        return evaluate(context, right_expr)
    else:
        return left_value

def evaluate_Equality(context, query):
    # Equality operator: returns true if any of the values in its left-hand
    # collection are equal to any of the values in its right-hand collection
    return [binary_comparison(context, query.left, query.right, lambda x, y: x == y)]

def evaluate_Inequality(context, query):
    return [binary_comparison(context, query.left, query.right, lambda x, y: x == y)]

def evaluate_GreaterThan(context, query):
    # Greater-than operator: same thing as equality, but checks to see if the
    # left-hand values are greater than the right-hand values instead of
    # equal. TODO: consider automatic conversion of strings to numbers
    # during comparisons if one side is already a number
    return [binary_comparison(context, query.left, query.right, lambda x, y: x > y)]

def evaluate_LessThan(context, query):
    return [binary_comparison(context, query.left, query.right, lambda x, y: x < y)]

def evaluate_GreaterOrEqual(context, query):
    return [binary_comparison(context, query.left, query.right, lambda x, y: x >= y)]

def evaluate_LessOrEqual(context, query):
    return [binary_comparison(context, query.left, query.right, lambda x, y: x <= y)]

def evaluate_And(context, query):
    left_value = evaluate(context, query.left)
    if is_true_collection(left_value):
        return [Boolean(is_true_collection(evaluate(context, query.right)))]
    else:
        return [Boolean(False)]

def evaluate_Or(context, query):
    left_value = evaluate(context, query.left)
    if not is_true_collection(left_value):
        return [Boolean(is_true_collection(evaluate(context, query.right)))]
    else:
        return [Boolean(True)]

def evaluate_PairConstructor(context, query):
    # Evaluate the left and right-hand sides
    left_value = evaluate(context, query.left)
    right_value = evaluate(context, query.right)
    # Now we check for length. A pair constructor's left-hand side and
    # right-hand side have to be the same length, and that many newly-created
    # pairs will be returned from the pair constructor.
    if len(left_value) != len(right_value):
        # Left-hand side and right-hand side don't have the same number of
        # items, so we throw an exception.
        raise Exception("The length of the collections on either side of a "
                "pair constructor must be the same. However, they were " + 
                str(len(left_value)) + " and " + str(len(right_value)) + 
                " for the key and the value, respectively.")
    # Left-hand side and right-hand side do have the same number of pairs, so
    # we construct one pair for each pair of items in the two sides.
    return [Pair(k, v) for k, v in zip(left_value, right_value)]

def evaluate_CollectionConstructor(context, query):
    # This one is fairly easy: we evaluate the left and right sides, then
    # concatenate the resulting collections and return our newly-created
    # collection. TODO: for efficiency, consider having collection
    # construction represent a series of commas with just one AST token
    # instead of an AST token for every comma.
    left_value = evaluate(context, query.left)
    right_value = evaluate(context, query.right)
    return left_value + right_value

def evaluate_IfThenElse(context, query):
    condition_value = evaluate(context, query.condition)
    if is_true_collection(condition_value):
        return evaluate(context, query.true)
    else:
        return evaluate(context, query.false)

def evaluate_Satisfies(context, query):
    name = query.name
    expr_value = evaluate(context, query.expr)
    some = query.type == "some"
    for item in expr_value:
        result = evaluate(context.new_with_var(name, [item]), query.condition)
        truth = is_true_collection(result)
        if some: # Checking for at least one to be true
            if truth:
                return [Boolean(True)]
        else: # Checking for all to be true, so if one's false, we just quit
            if not truth:
                return [Boolean(False)]
    if some: # Checking for some, but none of them were true
        return [Boolean(False)]
    else: # Checking for all, and none of them were false so we return true
        return [Boolean(True)]

def evaluate_Flwor(context, query):
    # Flwors are interesting constructs. The way I've done them here
    # essentially follows XQuery's notion of a tuple stream that they use to
    # define the behavior of the XQuery Flwor, except that we actually
    # represent the so-called tuples as dictionaries since this is more what
    # Python uses.
    # So, flwor evaluation uses generator composition to tie all of the
    # constructs together. For each construct, the corresonding flwor_*
    # generator is invoked, passing in the generator created for the preceding
    # flwor construct (or the result of invoking flwor_init, which yields one
    # empty map representing an empty varset, for the first construct). After
    # all of these generators are created, flwor_return, which is also a
    # generator, is invoked. It's similar to the other generators but it
    # yields collections, each of which corresponds to an invocation of the
    # return clause under the varset generated by the generator passed into
    # it. We then merge these into a single collection and we have our result.
    # So, first thing is to get the list of constructs in the flwor.
    constructs = query.constructs
    # Now we create an initial generator containing a single, empty map.
    last_generator = flwor_init()
    # Now we compose the constructs' generators together.
    for construct in constructs:
        # Look up which generator function to use for this construct
        typename = type(construct).__name__
        generator = getattr(jpath.engine, "flwor_" + typename, None)
        # Did we find a generator?
        if generator is None:
            # Nope, we haven't implemented this construct yet
            raise Exception("Flwor generator not implemented for construct " 
                    + typename + " containing " + trimTo(200, repr(construct))
                     + ". " + JPATH_INTERNAL_ERROR_MESSAGE)
        # Yep, we did. We'll create an instance of it, passing in the last
        # generator we created as this generator's source of varsets.
        last_generator = generator(context, construct, last_generator)
    # We've got the generators all tied together. Now we start iterating
    # through the results, merge them into a collection, and return it.
    return [item for collection in last_generator for item in collection]

def flwor_init():
    """
    A generator function that yields a single value, an empty map. This is
    used as the initial generator for a flwor to represent a single empty
    varset.
    """
    yield {}

def flwor_FlworFor(context, query, var_stream):
    # Get the name of the variable to store the value in, the name of the
    # variable to store the current index in, and the parsed expression to
    # evaluate to generate the collection of values to iterate over
    name = query.name
    counter = query.counter
    expr = query.expr
    # Now we iterate over all the existing varsets
    for varset in var_stream:
        # We evaluate the expression in this varset to get the collection of
        # items to iterate over
        expr_value = evaluate(context.new_with_vars(varset), expr)
        # Then we iterate over each of those items
        for index, item in enumerate(expr_value):
            # We create a new varset for this item, containing the variable
            # holding the item itself and optionally the variable holding the
            # index of this item in the collection
            new_varset = dict(varset)
            new_varset.update({name: [item]})
            if counter:
                new_varset.update({counter: [Number(index + 1)]})
            # Then we yield this varset, and we're good to go!
            yield new_varset

def flwor_FlworLet(context, query, var_stream):
    # This one's fairly easy. We start out by getting the name of the variable
    # we're creating and the expression whose value we're going to store in
    # this variable.
    name = query.name
    expr = query.expr
    # Then we iterate over the existing varsets
    for varset in var_stream:
        # For each existing varset, we evaluate the expression in this varset
        expr_value = evaluate(context.new_with_vars(varset), expr)
        # We then create a new varset for it, containing the variable this let
        # construct is declaring
        new_varset = dict(varset)
        new_varset.update({name: expr_value})
        # Then we yield the new varset, and that's it!
        yield new_varset

def flwor_FlworWhere(context, query, var_stream):
    # This one's even easier. We get the expression to evaluat to check
    # whether or not we want to allow each varset through
    expr = query.expr
    # Then we iterate over the varsets
    for varset in var_stream:
        # For each one, we evaluate the expression to see if we should let
        # this varset on to further processing stages
        expr_value = evaluate(context.new_with_vars(varset), expr)
        # Then we check to see if the result was a true value
        if is_true_collection(expr_value):
            # It was, so we yield this varset.
            yield varset
        # It wasn't, so we don't do anything with this varset, and we move on
        # to the next one

def flwor_FlworReturn(context, query, var_stream):
    # This isn't the same as all the other flwor constructs; it yields
    # collections instead of maps representing varsets. It actually does the
    # running of the return construct under every varset and yields the
    # collections generated by each evaluation. So, we first get the
    # expression to evaluate
    expr = query.expr
    # Then we iterate over all the varsets
    for varset in var_stream:
        # For each one, we evaluate the return construct under that varset
        expr_value = evaluate(context.new_with_vars(varset), expr)
        # And then we yield the resulting collection, and that's it!
        yield expr_value

 







































