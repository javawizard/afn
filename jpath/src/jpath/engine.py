# coding=UTF-8

from pyparsing import Literal, OneOrMore, ZeroOrMore, Regex, MatchFirst
from pyparsing import Forward, operatorPrecedence, opAssoc, Suppress, Keyword
from pyparsing import Optional
from string import ascii_lowercase, ascii_uppercase
import pyparsing
import itertools
import math
# Importing ourselves; we end up using this to look up which function to call
# for each component of the AST
import jpath.engine #@UnresolvedImport

JPATH_INTERNAL_ERROR_MESSAGE = ("This is a JPath internal error; report this "
        "to the JPath developers and they'll get it fixed as soon as "
        "possible. (one of them is alex@opengroove.org in case you don't "
        "have any other information about how to get in touch with them.)")

keychars = ascii_lowercase + ascii_uppercase


escape_map = {"\\": "\\", '"': '"', "n": "\n", "r": "\r"}
varNameRegex = r"\$[a-zA-Z][a-zA-Z0-9]*"

def trimTo(length, text):
    if len(text) < length:
        return text
    return text[:length] + "..."

def stringify(parser):
    """
    Adds a parse action to the specified parser to turn its matched token
    list into a single concatenated string. 
    """
    return parser.addParseAction(lambda t: "".join(t))

class InfixSeriesSuffixAction(object):
    def __init__(self, index):
        self.index = index
        
    def __call__(self, tokens):
        return {"i": self.index, "t": tokens[0]}

def InfixSeries(initial, operators):
    """
    Returns a parser that parses infix expressions. All the operators are
    assumed to have the same precedence level. initial is the parser
    representing the start of the expression. Each operator is a 2-tuple of:
    
    A parser that should parse both the operator and the expression on its
    right-hand side. It should have a parse action that causes it to return
    one and only one token.
    
    A function that accepts two parameters. We'll call them x and y. Once all
    of the operators are parsed, InfixSeries reduces from left to right (I may
    add an option for building right-associative InfixSeries instances at some
    point) each of the generated parse tokens. Since the initial parse token
    contributes to this, the first reduction will be the first infix operator
    that occurs in the parsed string. This function is then called, passing in
    x = either the initial token if no reductions have yet taken place, or the
    output generated by the last reduction performed, and y = the token that
    this operator's parser actually matched. The return value will then be
    passed as x to the next reduction that takes place, and so on.
    
    Once these reductions are complete, the result is returned as the parse
    token. If only the initial parser matches, then no reductions take place,
    and the token output of this parser is the token output of the initial
    parser.
    """
    if len(operators) == 0: # No operators, so just return the initial parser
        return initial
    parser = initial
    suffix_parsers = []
    for index, (operator_parser, function) in enumerate(operators):
        suffix_parsers.append(operator_parser.addParseAction(InfixSeriesSuffixAction(index)))
    parser = parser + ZeroOrMore(reduce(lambda x, y: x | y, suffix_parsers))
    def parse_action(t):
        results = list(t)
        return reduce(lambda x, y: operators[y["i"]][1](x, y["t"]), results)
    parser.addParseAction(parse_action)
    return parser

def SKeyword(text):
    """
    A suppressed keyword using keychars as the keyword end chars.
    """
    return Suppress(Keyword(text, keychars))

def NKeyword(text):
    """
    A non-suppressed keyword using keychars as the keyword end chars.
    """
    return Keyword(text, keychars)

def createPatternOrSpecial(pattern):
    if pattern == "true":
        return BooleanLiteral(True)
    elif pattern == "false":
        return BooleanLiteral(False)
    elif pattern == "null":
        return NullLiteral()
    return Pattern(pattern)

class Production(object):
    pass

def production(name, *varnames, **transformations):
    def __init__(self, *args):
        for index, var in enumerate(varnames):
            setattr(self, var, transformations.get(var, lambda t: t)(args[index]))
    def __repr__(self):
        return "<" + name + " " + ", ".join([var + ": " + repr(getattr(self, var)) for var in varnames]) + ">"
    return type(name, (Production,), {"__init__": __init__, "__str__": __repr__, "__repr__": __repr__})
    
NumberLiteral = production("NumberLiteral", "value", value=lambda t: float(t))
StringLiteral = production("StringLiteral", "value")
VarReference = production("VarReference", "name")
BooleanLiteral = production("BooleanLiteral", "value")
NullLiteral = production("NullLiteral")
ContextItem = production("ContextItem")
Children = production("Children")
PairChildren = production("PairChildren")
Pattern = production("Pattern", "value")
PairPattern = production("PairPattern", "value")
ParenExpr = production("ParenExpr", "expr")
ListConstructor = production("ListConstructor", "expr")
EmptyListConstructor = production("EmptyListConstructor")
MapConstructor = production("MapConstructor", "expr")
EmptyMapConstructor = production("EmptyMapConstructor")

Indexer = production("Indexer", "expr")
PairIndexer = production("PairIndexer", "expr")

Path = production("Path", "left", "right")
Predicate = production("Predicate", "left", "right")

Multiply = production("Multiply", "left", "right")
Divide = production("Divide", "left", "right")
Add = production("Add", "left", "right")
Subtract = production("Subtract", "left", "right")
Otherwise = production("Otherwise", "left", "right")

Equality = production("Equality", "left", "right")
Inequality = production("Inequality", "left", "right")
GreaterThan = production("GreaterThan", "left", "right")
LessThan = production("LessThan", "left", "right")
GreaterOrEqual = production("GreaterOrEqual", "left", "right")
LessOrEqual = production("LessOrEqual", "left", "right")

And = production("And", "left", "right")
Or = production("Or", "left", "right")

PairConstructor = production("PairConstructor", "left", "right")

CollectionConstructor = production("CollectionConstructor", "left", "right")

Flwor = production("Flwor", "constructs")
FlworFor = production("FlworFor", "name", "counter", "expr")
FlworLet = production("FlworLet", "name", "expr")
FlworWhere = production("FlworWhere", "expr")
# Add order by, let the ordering thing potentially be any arbitrary expression
# but note that ordering may slow down quite a bit if it's not a var, also
# consider defining ordering to be stable
FlworReturn = production("FlworReturn", "expr")


pExpr = Forward()

pNumber = Regex("[+-]?[0-9]+(\.[0-9]+)?").addParseAction(lambda t: NumberLiteral("".join(t)))
pStringEscape = Regex(r"\\[a-zA-Z0-9]").addParseAction(lambda t: escape_map[t[0][1]])
pStringChar = Regex(r'[^\"\\]').leaveWhitespace() | pStringEscape
pString = stringify(Literal('"') + ZeroOrMore(pStringChar) + Literal('"')).addParseAction(lambda t: StringLiteral(t[0][1:-1]))
pVarReference = Regex(varNameRegex).addParseAction(lambda t: VarReference(t[0][1:]))
pPattern = Regex("[a-zA-Z][a-zA-Z0-9]*").addParseAction(lambda t: createPatternOrSpecial(t[0]))
pPairPattern = Regex("@[a-zA-Z][a-zA-Z0-9]*").addParseAction(lambda t: PairPattern(t[0][1:])) # [1:] removes the leading "@"
pContextItem = Literal(".").addParseAction(lambda t: ContextItem())
pChildren = Literal("*").addParseAction(lambda t: Children())
pPairChildren = Literal("@*").addParseAction(lambda t: PairChildren())
pParenExpr = (Suppress("(") + pExpr + Suppress(")")).addParseAction(lambda t: ParenExpr(t[0]))
pListConstructor = (Suppress("[") + pExpr + Suppress("]")).addParseAction(lambda t: ListConstructor(t[0]))
pEmptyListConstructor = (Literal("[") + Literal("]")).addParseAction(lambda t: EmptyListConstructor())
pMapConstructor = (Suppress("{") + pExpr + Suppress("}")).addParseAction(lambda t: MapConstructor(t[0]))
pEmptyMapConstructor = (Literal("{") + Literal("}")).addParseAction(lambda t: EmptyMapConstructor())
pAtom = ( pParenExpr | pEmptyListConstructor | pListConstructor
        | pEmptyMapConstructor | pMapConstructor | pNumber | pString
        | pVarReference | pPattern | pPairPattern | pContextItem
        | pChildren | pPairChildren )

pIndexer = (Suppress("#") + pAtom).addParseAction(lambda t: Indexer(t[0]))
pPairIndexer = (Suppress("@#") + pAtom).addParseAction(lambda t: PairIndexer(t[0]))
pIndividual = pIndexer | pPairIndexer | pAtom

pInfix = pIndividual

pInfix = InfixSeries(pInfix,
        [
            ((Suppress("/") + pInfix), Path), 
            ((Suppress("[") + pExpr + Suppress("]")), Predicate)
        ])

pInfix = InfixSeries(pInfix,
        [
            ((Suppress(Literal(u"×") | Keyword("times", keychars) | Keyword("mul", keychars)) + pInfix), Multiply),
            ((Suppress(Literal(u"÷") | Keyword("divided by", keychars) | Keyword("div", keychars)) + pInfix), Divide)
        ])

pInfix = InfixSeries(pInfix,
        [
            ((Suppress(Literal(u"+") | Keyword("plus", keychars) | Keyword("add", keychars)) + pInfix), Add),
            ((Suppress(Literal(u"-") | Keyword("minus", keychars) | Keyword("sub", keychars)) + pInfix), Subtract)
        ])

pInfix = InfixSeries(pInfix,
        [
            ((Suppress(Keyword("otherwise", keychars)) + pInfix), Otherwise)
        ])

pInfix = InfixSeries(pInfix,
        [
            ((Suppress(Literal(">=")) + pInfix), GreaterOrEqual),
            ((Suppress(Literal("<=")) + pInfix), LessOrEqual),
            ((Suppress(Literal(">")) + pInfix), GreaterThan),
            ((Suppress(Literal("<")) + pInfix), LessThan),
            ((Suppress(Literal("!=")) + pInfix), Inequality),
            ((Suppress(Literal("=")) + pInfix), Equality)
        ])

pInfix = InfixSeries(pInfix,
        [
            ((Suppress(Keyword("and", keychars)) + pInfix), And)
        ])

pInfix = InfixSeries(pInfix,
        [
            ((Suppress(Keyword("or", keychars)) + pInfix), Or)
        ])

pInfix = InfixSeries(pInfix,
        [
            ((Suppress(Literal(":")) + pInfix), PairConstructor)
        ])

pInfix = InfixSeries(pInfix,
        [
            ((Suppress(Literal(",")) + pInfix), CollectionConstructor)
        ])

pFlworFor = (SKeyword("for") + Regex(varNameRegex) + Optional(SKeyword("at") + Regex(varNameRegex), "") + SKeyword("in")
            + pInfix).addParseAction(lambda t: FlworFor(t[0][1:], t[1][1:], t[2]))
pFlworLet = (SKeyword("let") + Regex(varNameRegex) + Suppress(":=") + pInfix).addParseAction(lambda t: FlworLet(t[0][1:], t[1]))
pFlworWhere = (SKeyword("where") + pInfix).addParseAction(lambda t: FlworWhere(t[0]))
pFlworReturn = (Suppress(Keyword("return", keychars)) + pInfix).setParseAction(lambda t: FlworReturn(t[0]))
pFlwor = (OneOrMore(pFlworFor | pFlworLet | pFlworWhere) + pFlworReturn).addParseAction(lambda t: Flwor(list(t)))

pFlworOrInfix = pFlwor | pInfix

pExpr << (pFlworOrInfix)


class Result(object):
    """
    The return value of any evaluation. It has one attribute, value, whose
    value is a list representing the collection of results. It may have
    additional attributes in the future (for example, XQuery has an update
    list as part of the XQuery update extensions; such a feature would be
    added as an additional attribute here).
    """
    def __init__(self, value):
        if not isinstance(value, list):
            raise Exception("Result values must be lists representing the "
                    "collection of results")
        self.value = value

class Item(object):
    """
    A JSON item. This represents a single value that can be passed around a
    query and stored in a collection. This class might be considered the
    JPath implementation equivalent of Python's object.
    
    All of the direct subclasses contain a set of functions that are used to
    access the underlying data. The query engine should depend /only/ on these
    functions, and not on any incidental data (and particularly not on any
    functions whose names start with an underscore). This allows instances of
    subclasses with optimized behavior to be passed into a query. For example,
    a database might be represented as one huge list, and storing such a list
    in memory would be impractical. Large documents stored in the database
    would also be impractical to represent in memory. This allows custom
    implementations to, for example, continue to store them on disk and
    consult indexes to retrieve the appropriate information. 
    """
    pass

class Pair(Item):
    def __init__(self, key, value):
        self._key = key
        self._value = value
    
    def get_key(self):
        """
        Returns the key of this pair.
        """
        return self._key
    
    def get_value(self):
        """
        Returns the value of this pair.
        """
        return self._value

class Map(Item):
    def __init__(self, map):
        if not isinstance(map, dict):
            raise Exception("Value is not a dictionary. If there becomes a "
                    "use case for using non-dictionary mapping objects, "
                    "I'll remove this restriction. (You can remove it "
                    "yourself by monkey-patching jpath.engine.Map.__init__ "
                    "to accept one argument and assign it to self._map "
                    "without checking its type.)")
        self._map = map
    
    def get_keys(self):
        """
        Returns a list of all keys present in this map.
        """
        return self._map.keys()
    
    def get_values(self):
        """
        Returns a list of all values present in this map.
        """
        return self._map.values()
    
    def get_pairs(self):
        """
        Returns a list of pairs, each one corresponding to an entry in this
        map.
        """
        return [Pair(k, v) for k, v in self._map.iteritems()]
    
    def get_key_count(self):
        """
        Returns the number of keys present in this map.
        """
        return len(self._map)
    
    def get_value(self, key):
        """
        Returns the value corresponding to a particular key in this map. If
        there is no entry for the specified key, this should return None.
        """
        return self._map.get(key, None)
    
    def get_pair(self, key):
        """
        Returns the pair corresponding to the specified key.
        """
        if key in self._map:
            return Pair(key, self._map[key])
        else:
            return None

class List(Item):
    def __init__(self, data):
        if not isinstance(data, list):
            raise Exception("Value is not a list")
        self._data = data
    
    def get_item_range(self, start, end):
        """
        Returns a list of items from this list, starting at start, inclusive,
        and ending with end, exclusive, both of which are zero-based. If
        either of those indexes are out of range, they should be automatically
        constrained to be in range. If no items are within the specified
        range, the empty list should be returned.
        """
        return self._data[start:end]
    
    def get_items(self):
        """
        Returns a list of all items in this list.
        """
        # Is copying this really necessary? Ideally any code calling this
        # function wouldn't modify the result, but just to be safe (and
        # especially for compatibility with code outside of the JPath engine
        # that might use JPath objects directly), we're copying it for now.
        return self._data[:]
    
    def get_item_count(self):
        """
        Returns the number of items contained within this list.
        """
        return len(self._data)

class String(Item):
    def __init__(self, value):
        if not isinstance(value, basestring):
            raise Exception("The value of a string has to be either a str or "
                    "a unicode. If you want to supply your own custom "
                    "string-like object, subclass String and do it that way. ")
        self._value = value
    
    def get_value(self):
        """
        Returns the value of this string as a string or a unicode.
        """
        return self._value

class Number(Item):
    def __init__(self, value):
        self._value = float(value)
    
    def get_float(self):
        """
        Returns the value of this number as a double.
        """
        return self._value
    
    def is_whole(self):
        """
        Returns true if this number is a whole number, or false if this number
        has a fractional part.
        """
        return math.floor(self._value) == self._value

class Boolean(Item):
    def __init__(self, value):
        if not isinstance(value, Boolean):
            raise Exception("Value is not a boolean")
        self._value = value
    
    def get_value(self):
        """
        Returns the value of this boolean as a Python boolean.
        """
        return self._value

class Null(Item):
    pass

def python_to_jpath(data):
    """
    Converts a Python representation of JSON data (the data that a call to
    simplejson.loads would return) into the JSON data representation used by
    the JPath query engine internally. You'll need to call this on any values
    that you want to set into variables (or even the context item) when
    evaluating a query.
    
    The return value will be a subclass of Item.
    """
    pass

def jpath_to_python(data):
    """
    The opposite of python_to_jpath: converts a representation used internally
    into JSON data suitable for, as an example, passing to simplejson.dumps.
    """
    pass

def parse(text):
    results = list(pExpr.parseString(text, parseAll=True))
    if len(results) != 1:
        raise Exception("Problem while parsing results: precisely one "
                "result was expected, but " + str(len(results)) + " were "
                "provided by the parser")
    return results[0]



class Context(object):
    def __init__(self, item=Null(), vars=None):
        self.item = item
        self.vars = vars
        if self.vars is None:
            self.vars = {}
    
    def var(self, name):
        return self.vars[name]
    
    def new_with_vars(self, vars):
        for key, value in vars.items():
            if not isinstance(value, list):
                raise Exception("Variable values have to be collections "
                        "(represented in Python as lists). So, instead of, for "
                        "example, c.new_with_var('foo', Number(1)), do "
                        "c.new_with_var('foo', [Number(1)]). Specifically, "
                        "you just tried to assign a value of type " + 
                        str(type(value)) + " to the var " + str(key) + ".")
        new_vars = dict(self.vars)
        new_vars.update(vars)
        return Context(self.item, new_vars)
    
    def new_with_var(self, name, value):
        return self.new_with_vars({name: value})
    
    def new_with_item(self, item):
        if not isinstance(item, Item):
            raise Exception("The context item must be an instance of a "
                    "subclass of Item, not an instance of " + str(type(item)))
        return Context(item, self.vars)
    
    def __repr__(self):
        return "Context(" + repr(self.item) + ", " + repr(self.vars) + ")"

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

def binary_comparison(context, left_expr, right_expr, function):
    """
    Performs a binary comparison between the values of the two expressions.
    These expressions are each evaluated. Each item in the two resulting
    collections is compared with each item in the other of the two collections
    by calling function, which should be of the form lambda x, y: ... where x
    is a value resulting from the evaluation of left_expr and y is a value
    resulting from the evaluation of right_expr. function should return True
    or False. If it returns True for any pairs of items, this function will
    return True. Otherwise, this function will return False.
    """
    left_value = evaluate(context, left_expr)
    right_value = evaluate(context, right_expr)
    return any([function(x, y) for x in left_value for y in right_value])

def evaluate_input(context=Context(), loop=False):
    """
    Prompts for a line of text with raw_input(), then evaluates it as a query.
    The results will then be printed to stdout, along with a summary.
    
    If loop is true, the user will be prompted again for another query, and so
    on until they hit enter without entering a query.
    
    The queries will be run under the specified context.
    """
    while True:
        text = raw_input("Q>> ")
        if text == "":
            return
        results = evaluate(context, text)
        if len(results) == 0:
            print "No results."
        elif len(results) == 1:
            print "1 result:"
        else:
            print str(len(results)) + " results:"
        for result in results:
            print str(result)
        if not loop:
            return

def evaluate_file(context, filename):
    """
    Runs the specified file in the specified context. This reads the file's
    contents and calls evaluate on the resulting text.
    """
    return evaluate(context, parse_file(filename))

def parse_file(filename):
    """
    Reads the contents of the specified file and calls parse, passing in the
    file's text. The result of the call to parse is returned.
    """
    with open(filename) as file:
        return parse(file.read())

def eval_against_python_item(query, item=None):
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
    return jpath_to_python(evaluate(context), query)
    """
    if item is None:
        context = Context()
    else:
        context = Context(python_to_jpath(item))
    return jpath_to_python(evaluate(context), query)

def evaluate(context, query):
    """
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
    if isinstance(query, basestring): # Query is a string, not an AST
        # component, so we need to parse it before evaluating it
        query = parse(query) # Now it's an AST component.
    # Figure out the method to dispatch to based on the type of AST node that
    # this is
    typename = type(query).__name__
    function = getattr(jpath.engine, "evaluate_" + typename, None) # Get the
    # function on this module that's supposed to process this AST component
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
        value = context.item.get_value(query.value)
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
TODO: pick up here converting existing stuff to use the new JSON objects that subclass Item
def evaluate_PairPattern(context, query):
    if isinstance(context.item, dict):
        if query.value in context.item:
            return [Pair(query.value, context.item[query.value])]
    return []

def evaluate_ParenExpr(context, query):
    return evaluate(context, query.expr)

def evaluate_ListConstructor(context, query):
    return [list(evaluate(context, query.expr))] # Create a list containing
    # the items present in the collection to be evaluated

def evaluate_EmptyListConstructor(context, query):
    return [[]]

def evaluate_MapConstructor(context, query):
    collection = evaluate(context, query.expr)
    result = {}
    for pair in collection:
        if not isinstance(pair, Pair):
            raise Exception("Maps (JSON objects) can only be constructed "
                    "from collections containing only pairs. The collection "
                    "being used to construct this map, however, contains an "
                    "item of type " + str(type(pair)) + ": " + repr(pair))
        result[pair.key] = pair.value
    return [result]

def evaluate_EmptyMapConstructor(context, query):
    return [{}]

def evaluate_Indexer(context, query):
    value = evaluate(context, query.expr)
    if len(value) < 1:
        return []
    value = value[0]
    if(isinstance(context.item, list)):
        value = int(value)
        if(value < 1 or value > len(context.item)):
            return []
        else:
            return [context.item[value-1]]
    elif(isinstance(context.item, map)):
        if(value in context.item):
            return [context.item[value]]
        else:
            return []

def evaluate_Path(context, query):
    left_value = evaluate(context, query.left)
    result_collections = [evaluate(context.new_with_item(v), query.right) for v in left_value]
    return list(itertools.chain(*result_collections))

def evaluate_Predicate(context, query):
    left_value = evaluate(context, query.left)
    return [v for v in left_value if is_true_collection(evaluate(context.new_with_item(v), query.right))]

def evaluate_Equality(context, query):
    return [binary_comparison(context, query.left, query.right, lambda x, y: x == y)]

def evaluate_GreaterThan(context, query):
    return [binary_comparison(context, query.left, query.right, lambda x, y: x > y)]

def evaluate_PairConstructor(context, query):
    left_value = evaluate(context, query.left)
    right_value = evaluate(context, query.right)
    if len(left_value) != len(right_value):
        raise Exception("The length of the collections on either side of a "
                "pair constructor must be the same. However, they were " +
                str(len(left_value)) + " and " + str(len(right_value)) +
                " for the key and the value, respectively.")
    return [Pair(k, v) for k, v in zip(left_value, right_value)]

def evaluate_CollectionConstructor(context, query):
    left_value = evaluate(context, query.left)
    right_value = evaluate(context, query.right)
    return left_value + right_value


































