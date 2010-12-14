
"""
Let's figure out some things about how this is going to work...

Functions are passed around as Python functions. Numbers are represented by
decimal.Decimal instances. Strings and booleans are represented by basestring
and bool instances. Lists and maps are represented by Python lists and dicts.
(Tuples don't function correctly at present. I hope to add support for them at
some point in the future, but I haven't yet decided how to go about this.) null
and void are represented by libfact. Null and None, respectively.

Functions are registered to a Interpreter individually. They can be either
normal or Fact-aware. By default, a function registered to an interpreter will
be registered as a normal function. The function has to be decorated with
@libfact.function in order to be a Fact-aware function. Fact-aware functions
are different in two ways: they can specify (via arguments to the decorator)
which arguments should be passed as values and which arguments should be passed
as closures, and they have an additional first argument, an instance of
libfact.Context which allows the function access to variables and other context
information. Closures are passed as callable objects that can be called with
a single optional argument, the context under which the closure should be run.
If the context is not specified, the context passed into the function into
which the closure was passed will be used.

Closures return what they evaluate to when run. The return value of a function
is the value that the function call will evaluate to in the Fact script. Since
void is represented in python by None, a function that does not return a value
will evaluate to void.

The dot operator only works on maps and lists. For these, it subscripts them.
If the result is None, it converts it to Null. Using the dot operator on Null
evaluates to Null, so a list of dot operations can be chained without fear
that one of them being null will cause an error.

Variables in a context can be accessed and written by subscripting the context.
Internally, they're stored in the context's vars attribute, which can be any
subscriptable object. The context itself should be used where possible, though,
as it translates some values (for example, it translates int, long, and float
to Decimal).

The parser's architecture is a bit different from the original Fact parser.
Instead of having one function that parses function references, there are two
functions: one parses closures and one parses function calls. The parser
returns a callable object that accepts one required argument, namely a context
in which it's supposed to be run. These are all subclasses of Entity.

The subclasses of Entity are Sequence, VarReference, Literal, and FunctionCall.
Each one has a function, resolve, that takes the context in which it should be
resolved. There's a class, Closure, that wraps this and tracks a
previously-existing context. Closure is callable and accepts one optional
argument, a context that will override the context it was created with.
Instances of Closure are passe to Fact-aware functions that request arguments
be passed as closures.

Sequences resolve themselves by resolving each of their arguments in turn.
They then call implicit_cat on their arguments. This function performs an implicit
concatenation on its values. VarReferences resolve themselves by evaluating
each of their dot components in turn and returning the result after this. If
there are no components, the empty string is returned. Literals simply return
a string representing the contents they were created with. FunctionCalls
resolve their first argument, then use it to ask the Interpreter they were
created against for the Function instance representing the named function.
They then call it, passing in the entities representing the function's
arguments. The Function class automatically wraps these in Closure instances
as needed, resolves those that are to be passed as values to the function, and
calls the function.

There are two main functions in charge of factoid parsing: parse_closure and
parse_function. The former parses Literals and VarReferences (and backslash
escapes) into a Sequence until it hits a | or a }, at which point it stops and
returns the Sequence. The latter expects its first input character to be a {.
It drops it, then calls parse_closure repeatedly. In between each invocation,
it checks to see what the next character is. If it's |, it continues with the
invocations. If it's }, it consumes the } and returns. Both of those functions
expect lists in reverse order (I.E. the last character in the list is the first
character in the text to parse and so on; this is because removing from the
end of a list in Python is usually several orders of magnitude more efficient
than removing from the beginning of a list). The parse function expects a
string. It converts it to a reversed list and parses it as a closure. It also
throws an exception if there's any input left after parsing, which would
indicate an extraneous | or } character.
"""

from decimal import Decimal

class NullType(object):
    pass

Null = NullType()

class Interpreter(object):
    def __init__(self, install_defaults=True):
        """
        Creates a new interpreter. If install_defaults is true, all of the
        functions returned by make_default_functions will be automatically
        registered to this interpreter.
        """
        self.functions = {}
        if install_defaults:
            for function in _default_functions:
                self.register_function(function)
    
    def register_function(self, *args):
        """
        register_function(name=None, function)
        
        Registers the specified function. If name is specified, it's used as
        the name for the function. Otherwise, the function's fact_name
        attribute is used as the name, or __name__ if the function does not
        have any such attribute.
        """
        if len(args) == 1:
            function = args[0]
            try:
                name = function.fact_name
            except AttributeError:
                name = function.__name__
            if name is None:
                raise Exception("Function does not have a name")
        else:
            name, function = args
        self.functions[name] = function
    
    def lookup_function(self, name, context):
        if name not in self.functions:
            raise Exception("No such function: " + str(name))
        return Function(self.functions[name], context)
    
    def parse(self, text):
        chars = list(text)
        chars.reverse()
        result = self.parse_closure(chars)
        if chars:
            raise Exception("Extra | or } detected outside of a function call")
        return result
    
    def parse_function(self, chars):
        if chars[-1] != "{":
            raise Exception("Functions must start with {")
        items = []
        while chars.pop() != "}":
            items.append(self.parse_closure(chars))
        return FunctionCall(self, items)
    
    def parse_closure(self, chars):
        items = []
        while(chars):
            char = chars.pop()
            if char == "{":
                # Push the { back on
                chars.append("{")
                # ...and then parse it as a function
                items.append(self.parse_function(chars))
            elif char in "|}":
                # End of this argument
                chars.append(char) # Push back so that parse_function can tell
                # whether this was a | or a }
                break
            elif char == "%":
                # Var reference. We'll get all the chars up to the next %
                var = ""
                while chars[-1] != "%":
                    var += chars.pop()
                chars.pop() # Get rid of the trailing %
                items.append(VarReference(var.split(".")))
            else:
                if char == "\\": # Process backslash escapes
                    char = process_escape(chars)
                # Check to see if the last item is a literal. If so, we'll just
                # add this char to it to avoid creating one Literal per char
                if items and isinstance(items[-1], Literal):
                    items[-1].append(char)
                else: # Last item is not a literal, so we'll create a new one
                    items.append(Literal(char))
        if len(items) == 0:
            return Void()
        if len(items) == 1:
            return items[0]
        return Sequence(items)

class Context(object):
    def __init__(self, vars=None):
        """
        Creates a new context. The specified object, which should be
        subscriptable, will be used as the local variable store. If it's not
        specified, a newly-created dictionary will be used.
        """
        if vars is None:
            vars = {}
        self.vars = vars
        
    def __getitem__(self, item):
        return foreign_get(self.vars, item)
    
    def __setitem__(self, item, value):
        self.vars[item] = value
    
    def varcopy(self, vars):
        """
        Creates a new context that mirrors (and delegates to) this one but uses
        the variables in the specified subscriptable object instead. 
        """
    
    def create_static_container(self, vars):
        """
        Creates a new context that mirrors (and delegates to) this one but uses
        a new StaticVarContainer to hold its variables. The container will
        point back to this context's variables and will use the specified
        mapping object as the static variable set.
        """
        return Context(StaticVarContainer(self.vars, vars))

class Function(object):
    def __init__(self, function, context):
        self.function = function
        self.context = context
    
    def run(self, entities):
        try:
            preserve_list = self.function.fact_preserve
            preserve_other = self.function.fact_preserve_other
            use_context = self.function.fact_context
        except AttributeError:
            preserve_list = []
            preserve_other = False
            use_context = False
        args = []
        for index, entity in enumerate(entities):
            if index >= len(preserve_list):
                preserve = preserve_other
            else:
                preserve = preserve_list[index]
            if preserve:
                entity = Closure(entity, self.context)
            else:
                entity = entity.resolve(self.context)
            args.append(entity)
        if use_context:
            args[0:0] = [self.context]
        return self.function(*args)

class Closure(object):
    def __init__(self, entity, context):
        self.entity = entity
        self.context = context
    
    def __call__(self, context=None):
        if context is None:
            context = self.context
        return self.entity.resolve(context)

class Entity(object):
    def resolve(self, context):
        raise Exception("resolve not implemented by an entity")

class Void(object):
    def resolve(self, context):
        return None

class Literal(Entity):
    def __init__(self, text):
        self.text = text
    
    def resolve(self, context):
        return self.text
    
    def append(self, text):
        self.text += text

class VarReference(Entity):
    def __init__(self, strings):
        self.strings = strings
    
    def resolve(self, context):
        if len(self.strings) == 0:
            return ""
        value = context[self.strings[0]]
        for specifier in self.strings[1:]:
            value = foreign_get(value, specifier)
        return value

class FunctionCall(Entity):
    def __init__(self, interpreter, items):
        self.interpreter = interpreter
        self.items = items
    
    def resolve(self, context):
        function_name = self.items[0].resolve(context)
        if not isinstance(function_name, basestring):
            raise Exception("Function name has to be a string; it was "
                    + str(function_name))
        function = self.interpreter.lookup_function(function_name, context)
        return function.run(self.items[1:])

class Sequence(Entity):
    def __init__(self, items):
        self.items = items
    
    def resolve(self, context):
        return implicit_cat([i.resolve(context) for i in self.items])

class StaticVarContainer(object):
    def __init__(self, container, static_vars):
        self.container = container
        self.special_vars = static_vars
    
    def __getitem__(self, item):
        if item in self.special_vars:
            return self.special_vars[item]
        return self.container[item]
    
    def __setitem__(self, item, value):
        if item in self.special_vars:
            raise Exception("Assignment to a static variable is not allowed.")
        self.container[item] = value

def implicit_cat(values):
    values = [value for value in values if value is not None]
    if len(values) == 0:
        return None
    if len(values) == 1:
        return values[0]
    return "".join(str(value) for value in values)

def foreign_get(object, item):
    if object is None or object is Null:
        return None
    try:
        if isinstance(object, (list, tuple)):
            item = int(item)
        return foreign_translate(object[item])
    except (KeyError, IndexError):
        return None

def foreign_translate(object):
    if object == None:
        return Null
    if isinstance(object, (int, long, float)):
        return Decimal(object)
    return object

def process_escape(chars):
    char = chars.pop()
    if char == "n":
        return "\n"
    if char == "r":
        return "\r"
    if char == "t":
        return "\t"
    if char == "x":
        return chr(int(chars.pop() + chars.pop(), 16))

def function(*args, **kwargs):
    """
    Decorator that can be applied to functions that are to be registered with
    Interpreter.register_function. The decorator modifies the function
    in-place instead of creating a new one. Each positional argument
    corresponds to a possible argument to the function. If it's False, as is
    the default (both when extra arguments are provided and this decorator is
    not present), that particular argument will be resolved and passed in as
    a value. If it's True, the arguments will be preserved as a closure that
    will then be passed in. The closure will be a callable object accepting
    one additional optional parameter, which is the context under which the
    closure should be evaluated. If it's not specified, the context in which
    this function was called will be used.
    
    Three keyword arguments can additionally be present: other, name, and
    context. Other specifies whether or not any additional variables provided
    to the function will be preserved as closures. Name specifies the name of
    a function, which will default to the Python name of the function if
    unspecified. Context is a boolean indicating whether the context under
    which the function is being evaluated will be passed as the first
    argument (and the first Fact argument to the function passed as the second
    argument, etc). It defaults to True if this annotation is present but
    context is unspecified and False if this annotation is not present at all.
    """
    other = kwargs.get("other", None)
    name = kwargs.get("name", None)
    context = kwargs.get("context", True)
    def decorator(function):
        function.fact_preserve = args
        function.fact_preserve_other = other
        if name is not None:
            function.fact_name = name
        function.fact_context = context
        return function
    return decorator

def make_default_functions():
    @function(name="+", context=False)
    def _add(*args):
        return reduce(lambda x, y: x + y, (Decimal(i) for i in args))

    @function(name="-", context=False)
    def _subtract(*args):
        return reduce(lambda x, y: x - y, (Decimal(i) for i in args))
    
    @function(name="*", context=False)
    def _multiply(*args):
        return reduce(lambda x, y: x * y, (Decimal(i) for i in args))
    
    @function(name="/", context=False)
    def _divide(*args):
        return reduce(lambda x, y: x / y, (Decimal(i) for i in args))

    @function(name="//", context=False)
    def _floordiv(*args):
        return reduce(lambda x, y: x // y, (Decimal(i) for i in args))
    
    @function(name="2pad0", context=False)
    def _twopad0(value):
        return str(value).rjust(2, "0")
    
    def identity(value):
        return value
    
    @function(False, context=False, other=True)
    def switch(value, *args):
        for i in range(0, len(args) - 1, 2): # Iterate over every pair of
            # elements, excluding the odd last element if there is one
            if args[i]() == value:
                return args[i + 1]()
        if (len(args) % 2) == 1:
            return args[-1]()
        else:
            return None
    
    def extrange(value, div, mod):
        """
        Syntax: {extrange|<value>|<div>|<mod>} -- Divides <value> by <div>,
        then evaluates to that result modulo <mod>. This is useful for
        programs such as a timer client; assuming <x> holds a possibly-large
        number of seconds, {extrange|<x>|1|60} would extract the number of
        seconds for display on a digital clock, {extrange|<x>|60|60} would
        extract the number of minutes, and {extrange|<x>|3600|60} would extract
        the hours.
        """
        return (Decimal(value) // Decimal(div)) % Decimal(mod)
    
    @function(False, False, False, True, False, name="for")
    def for_function(context, start, stop, var, action, delimiter):
        varmap = {}
        new_context = context.create_static_container(varmap)
        result = []
        for i in range(int(start), int(stop) + 1):
            varmap[var] = i
            result.append(action(new_context))
            result.append(delimiter)
        if result:
            result = result[:-1]
        return implicit_cat(result)
    
    @function(name="list", context=False, other=False)
    def _list(*args):
        return list(args)
    
    @function(False, False, name="set")
    def _set(context, name, value):
        context[name] = value
    
    @function(False)
    def get(context, name):
        return context[name]
    
    return [_v for _k, _v in locals().copy().items()
            if not (_k.startswith("__") or _k.startswith("_["))]
    # The cukoobirdie syntax with checking for _[ is that for some totally
    # random reason CPython seems to leak the list comprehension currently in
    # progress into locals() as _[1] or some such. Does anyone know what the
    # heck is up with that?

_default_functions = make_default_functions()


































