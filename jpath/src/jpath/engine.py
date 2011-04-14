
from pyparsing import Literal, OneOrMore, ZeroOrMore, Regex, MatchFirst
from pyparsing import Forward, operatorPrecedence, opAssoc
import pyparsing


escape_map = {"\\": "\\", '"': '"', "n": "\n", "r": "\r"}

def stringify(parser):
    """
    Adds a parse action to the specified parser to turn its matched token
    list into a single concatenated string. 
    """
    return parser.addParseAction(lambda t: "".join(t))

def ignore(parser):
    return parser.addParseAction(lambda t: [])

def IgnoredLiteral(char):
    return ignore(Literal(char))

def createPatternOrSpecial(pattern):
    if pattern == "true":
        return Boolean(True)
    elif pattern == "false":
        return Boolean(False)
    elif pattern == "null":
        return Null()
    return Pattern(pattern)

def datatype(name, *varnames, **transformations):
    def __init__(self, *args):
        for index, var in enumerate(varnames):
            setattr(self, var, transformations.get(var, lambda t: t)(args[index]))
    def __repr__(self):
        return "<" + name + " " + ", ".join([var + ": " + repr(getattr(self, var)) for var in varnames]) + ">"
    return type(name, (object,), {"__init__": __init__, "__str__": __repr__, "__repr__": __repr__})
    
Number = datatype("Number", "value", value=lambda t: float(t))

String = datatype("String", "value")

VarReference = datatype("VarReference", "name")

Boolean = datatype("Boolean", "value")

Null = datatype("Null") 

Pattern = datatype("Pattern", "value")

PairPattern = datatype("PairPattern", "value")

Indexer = datatype("Indexer", "expr")

PairIndexer = datatype("PairIndexer", "expr")

ParenExpr = datatype("ParenExpr", "expr")

ListConstructor = datatype("ListConstructor", "expr")

MapConstructor = datatype("MapConstructor", "expr")

OperationSet = datatype("OperationSet", "operations")

pExpr = Forward()
pNumber = Regex("[+-]?[0-9]+(\.[0-9]+)?").addParseAction(lambda t: Number("".join(t)))
pStringEscape = Regex(r"\\[a-zA-Z0-9]").addParseAction(lambda t: escape_map[t[0][1]])
pStringChar = Regex(r'[^\"\\]')
pString = stringify(Literal('"') + ZeroOrMore(pStringChar) + Literal('"')).addParseAction(lambda t: String(t[0][1:-1]))
pVarReference = Regex(r"\$[a-zA-Z][a-zA-Z0-9]*").addParseAction(lambda t: VarReference(t[0][1:]))
pPattern = Regex("[a-zA-Z][a-zA-Z0-9]*").addParseAction(lambda t: createPatternOrSpecial(t[0]))
pPairPattern = Regex("@[a-zA-Z][a-zA-Z0-9]").addParseAction(lambda t: PairPattern(t[0]))
pParenExpr = (IgnoredLiteral("(") + pExpr + IgnoredLiteral(")")).addParseAction(lambda t: ParenExpr(t[0]))
pListConstructor = (IgnoredLiteral("[") + pExpr + IgnoredLiteral("]")).addParseAction(lambda t: ListConstructor(t[0]))
pMapConstructor = (IgnoredLiteral("{") + pExpr + IgnoredLiteral("}")).addParseAction(lambda t: MapConstructor(t[0]))
pAtom = ( pParenExpr | pListConstructor | pMapConstructor | pNumber | pString
        | pVarReference | pPattern | pPairPattern)

pIndexer = (IgnoredLiteral("#") + pAtom).addParseAction(lambda t: Indexer(t[0]))
pPairIndexer = (IgnoredLiteral("@#") + pAtom).addParseAction(lambda t: PairIndexer(t[0]))
pIndexerOrAtom = pIndexer | pPairIndexer | pAtom

pPathSuffix = (IgnoredLiteral("/") + pAtom)
pPredicateSuffix = (IgnoredLiteral("[") + pExpr + IgnoredLiteral("]"))
pPathOperator = pAtom + ZeroOrMore(pPathSuffix | pPredicateSuffix)

TODO: group the path suffixes and predicate suffixes into actual datatypes

pExpr << (pPathOperator)











































