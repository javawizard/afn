
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

def filter_dict(input, rule_map):
    """
    Creates a new dictionary containing one key for each key that's present in
    both input and rule_map, which should both be dictionaries. The key
    present in the output dictionary will be the value of the corresponding
    entry in rule_map.
    
    In other words, this copies input, removes all keys not present in
    rule_map, then renames them to be the values of the matching keys in
    rule_map.
    """
    new = {}
    for old_key, new_key in rule_map.items():
        if old_key in input:
            new[new_key] = input[old_key]
    return new








