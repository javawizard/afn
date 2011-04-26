# coding=UTF-8

from pyparsing import Literal, OneOrMore, ZeroOrMore, Regex
from pyparsing import Forward, Suppress, Keyword
from pyparsing import Optional
from string import ascii_lowercase, ascii_uppercase

"""
This module provides the JPath parser used for query evaluation. It can be
used separately from the JPath query engine to manipulate parsed queries
similar to how Python's ast module can be used to manipulate parsed Python
scripts.
"""

keychars = ascii_lowercase + ascii_uppercase


escape_map = {"\\": "\\", '"': '"', "n": "\n", "r": "\r"}
varNameRegex = r"\$[a-zA-Z][a-zA-Z0-9\\-\\_]*"

def stringify(parser):
    """
    Adds a parse action to the specified parser to turn its matched token
    list into a single concatenated string. 
    """
    return parser.addParseAction(lambda t: "".join(t))

class InfixSeriesSuffixAction(object):
    def __init__(self, index):
        self.index = index
        
    def __call__(self, location, tokens):
        return {"i": self.index, "l": location, "t": tokens[0]}

def InfixSeries(initial, operators):
    """
    Returns a parser that parses infix expressions. All the operators are
    assumed to have the same precedence level. initial is the parser
    representing the start of the expression. Each operator is a 2-tuple of:
    
    A parser that should parse both the operator and the expression on its
    right-hand side. It should have a parse action that causes it to return
    one and only one token.
    
    A function that accepts three parameters. We'll call them l, x, and y. Once all
    of the operators are parsed, InfixSeries reduces from left to right (I may
    add an option for building right-associative InfixSeries instances at some
    point) each of the generated parse tokens. Since the initial parse token
    contributes to this, the first reduction will be the first infix operator
    that occurs in the parsed string. This function is then called, passing in
    x = either the initial token if no reductions have yet taken place, or the
    output generated by the last reduction performed, and y = the token that
    this operator's parser actually matched. The return value will then be
    passed as x to the next reduction that takes place, and so on. l is the
    starting position at which the whole infix expression matched. TODO:
    document l more.
    
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
    def parse_action(start_location, t):
        results = list(t)
        return reduce(lambda x, y: operators[y["i"]][1](start_location, x, y["t"]), results)
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


def createPatternOrSpecial(location, pattern):
    if pattern == "true":
        return BooleanLiteral(location, True)
    elif pattern == "false":
        return BooleanLiteral(location, False)
    elif pattern == "null":
        return NullLiteral(location)
    return Pattern(location, pattern)


def aCreateFunctionCall(location, tokens):
    name = tokens[0]
    expr = tokens[1]
    if isinstance(expr, CollectionConstructor): # Expand to function arguments
        return FunctionCall(location, name, expr.exprs)
    return FunctionCall(location, name, [expr])


class Production(object):
    """
    Superclass of all production classes created by the production function.
    
    self.p_varnames and self.p_transformations will be defined by the
    production function when creating a new subclass. self.p_name is also
    defined this way.
    
    self.p_query is set by the parse function.
    """
    def __init__(self, location, *args):
        self.p_query = None
        self.parse_location = location
        for index, var in enumerate(self.p_varnames):
            setattr(self, var, self.p_transformations.get(var, lambda t: t)(args[index]))
    
    def __repr__(self):
        if not self.p_varnames: # Production with no variables
            return "<" + self.p_name + ">"
        # Production has some variables
        return "<" + self.p_name + " " + ", ".join(
                [var + ": " + repr(getattr(self, var)) for var in self.p_varnames]) + ">"
    

def production(name, *varnames, **transformations):
    return type(name, (Production,), 
            {"p_name": name, 
             "p_varnames": varnames, 
             "p_transformations": transformations})
    
NumberLiteral = production("NumberLiteral", "value", value=lambda t: float(t))
StringLiteral = production("StringLiteral", "value")
VarReference = production("VarReference", "name")
BooleanLiteral = production("BooleanLiteral", "value")
NullLiteral = production("NullLiteral")
ContextItem = production("ContextItem")
Children = production("Children")
PairChildren = production("PairChildren")
FunctionCall = production("FunctionCall", "name", "exprs")
Pattern = production("Pattern", "value")
PairPattern = production("PairPattern", "value")
ParenExpr = production("ParenExpr", "expr")
ListConstructor = production("ListConstructor", "expr")
EmptyListConstructor = production("EmptyListConstructor")
MapConstructor = production("MapConstructor", "expr")
EmptyMapConstructor = production("EmptyMapConstructor")
EmptyCollectionConstructor = production("EmptyCollectionConstructor")

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

CollectionConstructor = production("CollectionConstructor", "exprs") # This is
# defined differently because a function call is essentially defined to be
# simply the function name, an open paren, an expr, and a close paren, and
# the function call parser examines the expr to be passed to the function and
# if it's a collection constructor, it expands it as the function's arguments.
# Storing the values as a list instead of a bunch of composed collection
# constructors makes this a lot easier.

IfThenElse = production("IfThenElse", "condition", "true", "false")
Satisfies = production("Satisfies", "type", "name", "expr", "condition")

Flwor = production("Flwor", "constructs")
FlworFor = production("FlworFor", "name", "counter", "expr")
FlworLet = production("FlworLet", "name", "expr")
FlworWhere = production("FlworWhere", "expr")
# Add order by, let the ordering thing potentially be any arbitrary expression
# but note that ordering may slow down quite a bit if it's not a var, also
# consider defining ordering to be stable
FlworReturn = production("FlworReturn", "expr")

DeclareVariable = production("DeclareVariable", "name", "expr")
DeclareFunction = production("DeclareFunction", "name", "args", "expr")
DeclareOption = production("DeclareOption", "name", "expr")
ImportModule = production("ImportModule", "module", "name")


pExpr = Forward()

pNumber = Regex("[+-]?[0-9]+(\.[0-9]+)?").addParseAction(lambda l, t: NumberLiteral(l, "".join(t)))
pStringEscape = Regex(r"\\[a-zA-Z0-9]").addParseAction(lambda l, t: escape_map[t[0][1]])
pStringChar = Regex(r'[^\"\\]').leaveWhitespace() | pStringEscape
pString = stringify(Literal('"') + ZeroOrMore(pStringChar) + Literal('"')).addParseAction(lambda l, t: StringLiteral(l, t[0][1:-1]))
pVarReference = Regex(varNameRegex).addParseAction(lambda l, t: VarReference(l, t[0][1:]))
pFunctionCall = (Regex("[a-zA-Z][a-zA-Z0-9\\-\\_\\.]*") + Suppress("(") + pExpr + Suppress(")")).addParseAction(aCreateFunctionCall)
pPattern = Regex("[a-zA-Z][a-zA-Z0-9\\-\\_]*").addParseAction(lambda l, t: createPatternOrSpecial(l, t[0]))
pPairPattern = Regex("@[a-zA-Z][a-zA-Z0-9]*").addParseAction(lambda l, t: PairPattern(l, t[0][1:])) # [1:] removes the leading "@"
pContextItem = Literal(".").addParseAction(lambda l, t: ContextItem(l))
pChildren = Literal("*").addParseAction(lambda l, t: Children(l))
pPairChildren = Literal("@*").addParseAction(lambda l, t: PairChildren(l))
pParenExpr = (Suppress("(") + pExpr + Suppress(")")).addParseAction(lambda l, t: ParenExpr(l, t[0]))
pListConstructor = (Suppress("[") + pExpr + Suppress("]")).addParseAction(lambda l, t: ListConstructor(l, t[0]))
pEmptyListConstructor = (Literal("[") + Literal("]")).addParseAction(lambda l, t: EmptyListConstructor(l))
pMapConstructor = (Suppress("{") + pExpr + Suppress("}")).addParseAction(lambda l, t: MapConstructor(l, t[0]))
pEmptyMapConstructor = (Literal("{") + Literal("}")).addParseAction(lambda l, t: EmptyMapConstructor(l))
pEmptyCollectionConstructor = (Literal("(") + Literal(")")).addParseAction(lambda l, t: EmptyCollectionConstructor(l))
pAtom = (pParenExpr | pEmptyListConstructor | pListConstructor
        | pEmptyMapConstructor | pMapConstructor | pEmptyCollectionConstructor
        | pNumber | pString | pVarReference | pFunctionCall | pPattern 
        | pPairPattern | pContextItem | pChildren | pPairChildren)

pIndexer = (Suppress("#") + pAtom).addParseAction(lambda l, t: Indexer(l, t[0]))
pPairIndexer = (Suppress("@#") + pAtom).addParseAction(lambda l, t: PairIndexer(l, t[0]))
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

# NOTE: This doesn't always end up as a CollectionConstructor; if one of these
# appears inside a FunctionCall, it gets expanded to be the function call's
# arguments. See the definition of pFunctionCall for more information on what
# ends up happening.
pInfix = (pInfix + ZeroOrMore(Suppress(",") + pInfix)).addParseAction(lambda l, t: t[0] if len(t) == 1 else CollectionConstructor(l, list(t)))

pIfThenElse = (SKeyword("if") + pExpr + SKeyword("then") + pExpr + SKeyword("else") + pExpr
        ).addParseAction(lambda l, t: IfThenElse(l, t[0], t[1], t[2]))

pSatisfies = ((NKeyword("some") | NKeyword("every")) + Regex(varNameRegex) + SKeyword("in") + pExpr + SKeyword("satisfies") + pExpr
        ).addParseAction(lambda l, t: Satisfies(l, t[0], t[1][1:], t[2], t[3]))

pFlworFor = (SKeyword("for") + Regex(varNameRegex) + Optional(SKeyword("at") + Regex(varNameRegex), "") + SKeyword("in")
            + pExpr).addParseAction(lambda l, t: FlworFor(l, t[0][1:], t[1][1:], t[2]))
pFlworLet = (SKeyword("let") + Regex(varNameRegex) + Suppress(":=") + pExpr).addParseAction(lambda l, t: FlworLet(l, t[0][1:], t[1]))
pFlworWhere = (SKeyword("where") + pExpr).addParseAction(lambda l, t: FlworWhere(l, t[0]))
pFlworReturn = (Suppress(Keyword("return", keychars)) + pExpr).setParseAction(lambda l, t: FlworReturn(l, t[0]))
pFlwor = (OneOrMore(pFlworFor | pFlworLet | pFlworWhere) + pFlworReturn).addParseAction(lambda l, t: Flwor(l, list(t)))


pExpr << (pIfThenElse | pSatisfies | pFlwor | pInfix)

pQuery = pExpr
