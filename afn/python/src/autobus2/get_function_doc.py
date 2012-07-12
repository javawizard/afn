"""
This was taken from libautobus.get_function_doc. I'm sticking it in its own
module for now; I'll think about where it belongs later.
"""

import inspect

def get_function_doc(interface, function_name, remove_self=True):
    function = getattr(interface, function_name)
    is_method_before = inspect.ismethod(function)
    while hasattr(function, "wrapped"):
        function = function.wrapped
    if is_method_before or inspect.ismethod(function):
        argspec = inspect.getargspec(function)
        # Remove the leading "self" argument
        if remove_self:
            argspec = (argspec[0][1:],) + argspec[1:]
        args = inspect.formatargspec(*argspec)
    elif inspect.isfunction(function):
        args = inspect.formatargspec(*inspect.getargspec(function))
    else:
        args = "(...)"
    doc = inspect.getdoc(function)
    if doc is None:
        doc = ""
    return function_name + args + "\n\n" + doc