
def slicer(length, start=None, stop=None, step=None):
    start, stop, step = slice(start, stop, step).indices(length)
    if(step > 0):
        while start < stop:
            yield start
            start += step
    else:
        while start > stop:
            yield start
            start += step


def field(function):
    name = function.__name__
    def get_function(self):
        return getattr(self, "_p_" + name)
    def set_function(self, value):
        setattr(self, "_p_" + name, value)
    p = property(get_function, set_function, doc=function.__doc__)
    return p


def full_name(some_class):
    if not isinstance(some_class, type): # This is an instance of a class, not
        # a class itself, so we need to get this object's class
        some_class = type(some_class)
    return some_class.__module__ + "." + some_class.__name__
