# coding=UTF-8

from parcon import (Literal, InfixExpr, SignificantLiteral, Forward,
        number as p_number, Alphanum, CharNotIn, concat, alpha_chars,
        digit_chars, CharIn, digit, Alpha, Upper, Lower, Optional, ZeroOrMore,
        Keyword, OneOrMore, flatten, Exact, AnyChar, Except, Return, Invalid,
        Whitespace)
from jpath.query.productions import *
from collections import namedtuple
import operator

string_escape_map = {"n": "\n", "r": "\r", "t": "\t", '"': '"', "'": "'"}

PathWrapper = namedtuple("PathWrapper", ("expr",))
PredicateWrapper = namedtuple("PredicateWrapper", ("expr",))

def keyword(text):
    return Keyword(SignificantLiteral(text), CharNotIn(alpha_chars))


def lookup_string_escape(char):
    return string_escape_map[char]


def create_function_call((name, args)):
    if args is None:
        return FunctionCall(name, [])
    if isinstance(args, SequenceConstructor):
        return FunctionCall(name, args.exprs)
    return FunctionCall(name, [args])


def create_pattern_or_special(text):
    if text == "true":
        return BooleanLiteral(True)
    if text == "false":
        return BooleanLiteral(False)
    if text == "null":
        return NullLiteral()
    return Pattern(text)


def create_path(sequence):
    value = sequence[0]
    for current in sequence[1:]:
        if isinstance(current, PathWrapper):
            value = Path(value, current.expr)
        elif isinstance(current, PredicateWrapper):
            value = Predicate(value, current.expr)
        else:
            raise NotImplementedError(type(current))
    return value


def create_xml_tag((name, attributes, contents, close_name)):
    if name != close_name:
        raise Exception('XML tag was closed with name "' + close_name + 
                '" but it was opened with name "' + name + '"')
    return XMLTag(StringLiteral(name), attributes, contents)


def create_empty_xml_tag((name, attributes)):
    return XMLTag(StringLiteral(name), attributes, [])


def create_sequence(exprs):
    if len(exprs) == 1:
        return exprs[0]
    return SequenceConstructor(exprs)


upper = Upper()(name="upper")
lower = Lower()(name="lower")
alpha = (upper | lower)(name="alpha")
alphanum = (alpha | digit)(name="alphanum")
literal_escape = ("\\" + AnyChar())
id_with_dot = Exact(+(alphanum | CharIn("-_.") | literal_escape))[concat](name="identifier with dot")
id_no_dot = Exact(+(alphanum | CharIn("-_") | literal_escape))[concat](name="identifier without dot")
a_id_with_dot = Exact(alpha + -+(alphanum | CharIn("-_.") | literal_escape))[concat](name="alpha identifier with dot")
a_id_no_dot = Exact(alpha + -+(alphanum | CharIn("-_") | literal_escape))[concat](name="alpha identifier without dot")

expr = Forward()

number = p_number[NumberLiteral]
string_escape = ("\\" + (alphanum | '\\' | '"'))[lookup_string_escape]
string_char = CharNotIn('"\\')(desc='any char except " or \\') | string_escape
string = Exact('"' + string_char[...][concat] + '"')(name="string")
string_literal = string[StringLiteral]
var_name = ("$" + id_no_dot)(name="variable")
var_reference = var_name[VarReference]
function_name = a_id_with_dot(name="function name")
function_call = (function_name + "(" + Optional(expr, (None,)) + ")")[create_function_call](name="function call")
pattern = a_id_with_dot[create_pattern_or_special](name="pattern")
pair_pattern = ("@" + a_id_with_dot)[PairPattern](name="pair pattern")
context_item = Literal(".")[lambda t: ContextItem()](name="context item")
children = Literal("*")[lambda t: Children()](name="children")
pair_children = Literal("@*")[lambda t: PairChildren()](name="pair children")
paren_expr = ("(" + expr + ")")[ParenExpr](name="expr in parentheses")
list_constructor = ("[" + expr + "]")[ListConstructor](name="list")
empty_list_constructor = (Literal("[") + "]")[lambda t: EmptyListConstructor()](name="empty list")
object_constructor = ("{" + expr + "}")[ObjectConstructor](name="object")
empty_object_constructor = (Literal("{") + "}")[lambda t: EmptyObjectConstructor()](name="empty object")
empty_sequence_constructor = (Literal("(") + ")")[lambda t: EmptySequenceConstructor()](name="empty sequence")

xml_node = Forward()
xml_start_char = (alpha | CharIn(":_"))
xml_name = (xml_start_char + (xml_start_char | digit | CharIn("-."))[...])[flatten](name="xml name")
xml_attribute = (xml_name + "=" + string_literal)[lambda (n, v): XMLAttribute(StringLiteral(n), v)](name="xml attribute")
xml_empty_tag = ("<" + xml_name + xml_attribute[...] + "/" + ">")[create_empty_xml_tag](name="xml empty tag")
xml_text = OneOrMore(CharNotIn("&<{")(desc="any char except & < {"))[concat][StringLiteral](name="xml text")
xml_expr = ("{" + expr + "}")(name="xml expr")
xml_contents = (xml_text | xml_node | xml_expr)[...](name="xml contents")
xml_tag = ("<" + xml_name + xml_attribute[...] + ">" + xml_contents + "<" + "/" + xml_name + ">")[create_xml_tag](name="xml tag")
xml_node << (xml_tag | xml_empty_tag)

atom = (paren_expr | xml_node | empty_list_constructor | list_constructor
       | empty_object_constructor | object_constructor
       | empty_sequence_constructor | number | string_literal | var_reference
       | function_call | pattern | pair_pattern | context_item | children
       | pair_children)(name="atom")

indexer = ("#" + atom)[Indexer](name="indexer")
pair_indexer = ("@#" + atom)[PairIndexer](name="pair indexer")

term = (pair_indexer | indexer | atom)(name="term")

infix = (term + Optional(OneOrMore(
            ("/" + term)[PathWrapper] | 
            ("[" + expr + "]")[PredicateWrapper]
        ), []))[flatten][create_path](name="path")

infix = InfixExpr(infix, [
        ("×" | keyword("times") | keyword("mul"), Multiply),
        ("÷" | keyword("div") | (keyword("divided") + keyword("by")), Divide)
        ])

infix = InfixExpr(infix, [
        ((Literal("+") - "++") | keyword("plus") | keyword("add"), Add),
        ("-" | keyword("minus") | keyword("sub"), Subtract)
        ])

infix = InfixExpr(infix, [
            (keyword("otherwise"), Otherwise)
        ])

infix = InfixExpr(infix, [
            ("++", Concatenate)
        ])

infix = InfixExpr(infix, [
            (">=", GreaterOrEqual),
            ("<=", LessOrEqual),
            (">", Greater),
            ("<", Less),
            ("!=", NotEqual),
            ("=", Equal),
        ])

infix = InfixExpr(infix, [
            (keyword("and"), And)
        ])

infix = InfixExpr(infix, [
            (keyword("or"), Or)
        ])

infix = InfixExpr(infix, [
            (":", PairConstructor)
        ])

infix = infix(name="infix")

infix = (infix + ("," + infix)[...])[flatten][create_sequence](name="sequence")

if_then_else = (~keyword("if") + expr + ~keyword("then") + expr + ~keyword("else") + expr
        )[lambda (a, b, c): IfThenElse(a, b, c)](name="if")

quantifier = ((keyword("some") | keyword("every")) + var_name + ~keyword("in") + expr + ~keyword("satisfies") + expr
        )[lambda (a, b, c, d): Quantifier(a, b, c, d)](name="quantifier")

flwor_for = (~keyword("for") + var_name + Optional(~keyword("at") + var_name, "")
        + ~keyword("in") + expr)[lambda a: FlworFor(*a)]
flwor_let = (~keyword("let") + var_name + ":=" + expr)[lambda a: FlworLet(*a)]
flwor_where = (~keyword("where") + expr)[FlworWhere]
flwor_order_by = (~keyword("order") + ~keyword("by") + expr)[FlworOrderBy]
flwor_at = (~keyword("at") + var_name)[FlworAt]
flwor_do = (~keyword("do") + expr)[FlworDo]
flwor_return = (~keyword("return") + expr)
flwor_construct = (flwor_for | flwor_let | flwor_where | flwor_order_by | flwor_at | flwor_do)
flwor = (+(flwor_construct) + flwor_return)[lambda (c, r): Flwor(c, r)](name="flwor")

insert_position = (keyword("before") + expr | keyword("after") + expr | 
        (Optional(~keyword("at") + (keyword("start") | keyword("end") | 
                ~keyword("position") + expr), (None,)) + ~keyword("into") + expr))
insert = (~keyword("insert") + expr + insert_position)[lambda a: Insert(*a)]
delete = (~keyword("delete") + 
        (~keyword("value") | ~keyword("values")) + expr)[Delete]
replace = (~keyword("replace") + ~keyword("value") + expr + ~keyword("with") + expr)[Replace]
merge = (~keyword("merge") + expr + ~keyword("into") + expr)[Merge]

update = (insert | delete | replace | merge)(name="update")

expr << (if_then_else | quantifier | update | flwor | infix)(name="expr")

function_arg_def = (Optional(keyword("closure"), "normal") + var_name + 
        Optional(":=" + atom, (None,)))[lambda a: FunctionDefArg(*a)]
function_arg_def_list = InfixExpr(function_arg_def[lambda x: [x]], [(",", operator.add)])(name="function argument list")
function_definition = (~keyword("define") + ~keyword("function") + function_name
        + "(" + Optional(function_arg_def_list, []) + ")" + expr + ~keyword("end"))[lambda a: FunctionDef(*a)](name="function definition")

string_or_literal = (string | Exact(+((AnyChar() - CharIn(" \r\n\t"))(desc="any non-whitespace char")))[concat])(name="string or embedded literal")

import_binder = Except(id_with_dot, keyword("as") | keyword("import"))(name="import binder")
import_source = Except(string_or_literal, keyword("as") | keyword("import"))(name="import source")
import_target = (a_id_no_dot)(name="import target")
import_statement = (~keyword("import") + (import_binder + import_source | Return((None,)) + import_source)
         + Optional(~keyword("as") + import_target, (None,)))[lambda a: Import(*a)](name="import")

option = (~keyword("option") + string_or_literal + string_or_literal)[lambda a: Option(*a)](name="option")

prolog = (option | import_statement)[...] + function_definition[...]

comment = Forward()
comment << Exact(Literal("(:") + ((AnyChar() - (Literal("(:") | Literal(":)"))) | comment)[...] + ":)")

comment_or_whitespace = Whitespace() | comment 

module = Exact((prolog + Optional(expr, EmptySequenceConstructor())), comment_or_whitespace)[lambda a: Module(*a)](name="query")




































