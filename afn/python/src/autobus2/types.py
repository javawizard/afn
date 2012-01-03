
"""
This module contains all of the decorators that can be used to specify the
types of parameters given to Autobus functions, events, and so on.

Decorating functions intended to be given to Autobus with type information
provides two useful things: the actual arguments to the function, when it is
called, will be type checked (thus guaranteeing that an object of the wrong
type won't be passed in on accident), and remote clients can ask a function for
the types of its arguments.
"""