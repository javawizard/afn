
"""
A module that provides basic static typing abilities to Python functions.

It's similar to the static module I wrote for Parcon, but it's a lot simpler;
parcon.static allowed rather complex type matching, whereas simplestatic will
only allow basic typing for now. (Think Java type signatures.)

The main reason this is being written is to provide a mechanism for specifying
type information on Autobus 2 functions while making the mechanism for type
enforcement and declaration self-contained so that it can be used outside of
Autobus.

It works something like this:

@params(int, basestring)
@returns(basestring)
def repeated(number, text):
    return text * number

@params specifies the types of the parameters to be passed into the function.
@returns specifies the return type; if no value is to be returned, @void should
be used instead. (The function must return None if it's annotated with @void.)

I'll probably switch this over to use Python 3's function annotation mechanism
once I convert this module (and Autobus and everything else that uses them)
over to Python 3. 
"""

from functools import wraps

def _wrap(function):
    """
    Wraps the specified function with a simplestatic type-validating function,
    then returns this newly-created wrapper. If the specified function is a
    wrapper previously returned from _wrap, it will be returned as-is.
    
    Wrappers have an attribute named simplestatic whose value is a dictionary;
    this dictionary can be modified to alter the type-validating behavior of
    the wrapper. Three keys can be present; params, remainder, and returns.
    params is a list (or other sequence) of types to validate each parameter
    against. remainder is a type to apply to any additional parameters; this is
    typically used for variable argument functions. returns is a type to apply
    to the return value of a function; this can be None to indicate that the
    function does not return a value.
    
    Two additional keys can be present:
    
    required: This is the number of arguments that are required.
    
    offset: This is the number of arguments at the beginning of the function
    that should be completely ignored. They will be essentially removed from
    the argument list before type checking and added back afterward. This can
    be used to skip the self argument passed to class methods; @method is an
    alternate spelling of @offset(1).
    
    """
    if hasattr(function, "simplestatic"):
        return function
    simplestatic = {}
    @wraps(function)
    def wrapper(*args, **kwargs):
        # Remove the number of arguments specified by offset before doing
        # anything else
        offset = simplestatic.get("offset", 0)
        initial, args = args[:offset], args[offset:]
        # Get the parameter list
        params = simplestatic.get("params", [])
        # Make sure we've got enough parameters. If required isn't specified,
        # we're not going to require any parameters. In the future, we might
        # want to introspect the function's default arguments to see how many
        # are required; the only problem with this is that if other decorators
        # are used on the function, we won't be able to get a proper idea of
        # what arguments are required.
        required = simplestatic.get("required", 0)
        if len(args) < required:
            # Not enough arguments were specified
            raise TypeError("%r requires %s arguments but only got %s" %
                    (function, required + offset, len(args) + len(initial)))
        # We've got enough arguments. Now pair up the parameters and the
        # arguments and make sure we've got the right types.
        for index, (param, param_type) in enumerate(zip(args, params)):
            if not isinstance(param, param_type):
                # Wrong type; raise an exception indicating so. TODO: Might
                # want to create a subclass of SemanticException and TypeError
                # specific to this error.
                raise TypeError("Argument %s to %r was supposed to be of type "
                        "%r but the value %r was received instead" % 
                        (index + len(initial), function, param_type, param))
        # All positional arguments (that were specified) have been checked for
        # correct type. Now check to see if remainder was specified, and check
        # the remaining arguments if it was.
        if "remainder" in simplestatic:
            # Remainder was specified, so check any additional arguments above
            # and beyond those mentioned in params to make sure they're of the
            # requested type.
            for index, a in enumerate(args[len(params):], len(initial) + len(params)):
                if not isinstance(a, simplestatic["remainder"]):
                    # Wrong type; raise an exception indicating so.
                    raise TypeError("Variadic argument %s to %r was supposed "
                            "to be of type %r but the value %r was received "
                            "instead" %
                            (index, function, simplestatic["remainder"], a))
        # There aren't any additional arguments or they all passed type
        # checking. We should be good to run the function now.
        result = function(*(initial + args), **kwargs)
        # Now we check to see if we're supposed to typecheck the result.
        if "returns" in simplestatic:
            # We're supposed to typecheck the result. First we'll see if the
            # result is supposed to be None but isn't.
            if simplestatic["returns"] is None and result is not None:
                # Result is supposed to be None but it isn't. Throw an
                # appropriate exception.
                raise TypeError("Result of function %r was supposed to be "
                        "None but was %r instead" %
                        (function, result))
            # Now we check to see if the result is supposed to be of a
            # particular type but isn't.
            elif not isinstance(result, simplestatic["returns"]):
                # Result wasn't of the correct type
                raise TypeError("Result of function %r was supposed to be of "
                        "type %r but the value %r was returned instead" %
                        (function, simplestatic["returns"], result))
        # Function passed type validation, so return it.
        return result
    wrapper.simplestatic = simplestatic
    return wrapper


def params(*p):
    """
    Specifies the types of the parameters that the function accepts.
    """
    def decorator(function):
        wrapper = _wrap(function)
        wrapper.simplestatic["params"] = p
        return wrapper
    return decorator


def required(number):
    def decorator(function):
        wrapper = _wrap(function)
        wrapper.simplestatic["required"] = number
        return wrapper
    return decorator


def returns(t):
    """
    Specifies the type that the function returns. Note that if the function
    always returns None, @void should be used instead of @returns(NoneType).
    """
    def decorator(function):
        wrapper = _wrap(function)
        wrapper.simplestatic["returns"] = t
        return wrapper
    return decorator


def void(function):
    """
    Specifies that the function has no return value. Functions decorated with
    @void must always return None.
    """
    wrapper = _wrap(function)
    wrapper.simplestatic["returns"] = None
    return wrapper


def remainder(t):
    """
    Specifies the type of the arguments not specified by @params.
    """
    def decorator(function):
        wrapper = _wrap(function)
        wrapper.simplestatic["remainder"] = t
        return wrapper
    return decorator


def offset(number):
    """
    Specifies the number of arguments at the beginning of the function to
    ignore while typechecking.
    """
    def decorator(function):
        wrapper = _wrap(function)
        wrapper.simplestatic["offset"] = number
        return wrapper
    return decorator


def method(function):
    """
    An alternate spelling of @offset(1).
    """
    wrapper = _wrap(function)
    wrapper.simplestatic["offset"] = 1
    return wrapper













