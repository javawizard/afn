
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