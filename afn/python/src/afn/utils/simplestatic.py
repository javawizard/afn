
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
    """
    if hasattr(function, "simplestatic"):
        return function
    simplestatic = {}
    @wraps(function)
    def wrapper(*args, **kwargs):
        # Get the parameter list
        params = simplestatic.get("params", [])
        # Make sure we've got enough parameters. If required isn't specified,
        # we're not going to require any parameters. In the future, we might
        # want to introspect the function's default arguments to see how many
        # are required; the only problem with this is that if other decorators
        # are used on the function, we won't be able to get a proper idea of
        # what arguments are required.
        required = simplestatic.get("required", len(params))
        if len(args) < required:
            
        return function(*args, **kwargs)
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













