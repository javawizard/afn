
from traceback import print_exc as _print_exc

class BlankObject(object):
    """
    A blank object that allows arbitrary assignment to its instances'
    attributes.
    """
    pass

def cast(instance, *types):
    """
    Raises an exception if isinstance(instance, types) returns False. This is
    useful in, for example, Autobus interfaces to ensure that objects passed
    in by a client are of the correct type.
    """
    if not isinstance(instance, types):
        raise Exception("The specified object's type is " + str(type(instance))
                + ", but it needs to be one of " + str(types))

class NoExceptions(object):
    def __enter__(self):
        pass
    
    def __exit__(self, *args):
        return True

class PrintExceptions(object):
    def __enter__(self):
        pass
    
    def __exit__(self, *args):
        _print_exc()








