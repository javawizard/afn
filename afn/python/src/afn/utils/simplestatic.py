
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
    the wrapper. Three keys can be present; params, remainder, and return.
    params is a list (or other sequence) of types to validate each parameter
    against. remainder is a type to apply to any additional parameters; this is
    typically used for variable argument functions.
    """
    if hasattr(function, "simplestatic"):
        return function
    simplestatic = {}
    @wraps(function)
    def wrapper(*args, **kwargs):
        return function(*args, **kwargs)
    wrapper.simplestatic = simplestatic
    return wrapper













