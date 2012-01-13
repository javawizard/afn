
class Partial(object):
    """
    A replacement for functools.partial that implements __eq__ and __ne__; a
    Partial object is equal to another Partial if their underlying functions
    are equal, both contain the same number of arguments and keyword arguments,
    and their arguments and keyword arguments compare equal.
    """
    def __init__(*args, **kwargs): #@NoSelf
        self = args[0]
        self.function = args[1]
        self.args = args[2:]
        self.kwargs = kwargs
    
    def __call__(*args, **kwargs): #@NoSelf
        self = args[0]
        args = args[1:]
        kwtemp = self.kwargs.copy()
        kwtemp.update(kwargs)
        return self.function(*(self.args + args), **kwtemp)
    
    def __eq__(self, other):
        if not isinstance(other, Partial):
            return NotImplemented
        return (self.function == other.function and 
                self.args == other.args and 
                self.kwargs == other.kwargs)
    
    def __ne__(self, other):
        if not isinstance(other, Partial):
            return NotImplemented
        return not self.__eq__(other)


def partial(*args, **kwargs):
    """
    A function that simply returns Partial(*args, **kwargs). This allows this
    module to function as a drop-in replacement for functools insofar as using
    partial goes.
    """
    return Partial(*args, **kwargs)





