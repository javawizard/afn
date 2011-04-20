
from jpath.data import String, Boolean, Number, Null, Map, Pair, List

def python_to_jpath(data):
    """
    Converts a Python representation of JSON data (the data that a call to
    simplejson.loads would return) into the JSON data representation used by
    the JPath query engine internally. You'll need to call this on any values
    that you want to set into variables (or even the context item) when
    evaluating a query.
    
    The return value will be a subclass of Item.
    """
    if isinstance(data, basestring):
        return String(data)
    elif isinstance(data, bool):
        return Boolean(data)
    elif isinstance(data, (int, long, float)):
        return Number(float(data))
    elif data is None:
        return Null()
    elif isinstance(data, dict):
        return Map([Pair(python_to_jpath(k), python_to_jpath(v)) for k, v in data.items()])
    elif isinstance(data, (tuple, list)):
        return List([python_to_jpath(v) for v in data])
    else:
        raise Exception("No python -> jpath encoding for " + str(type(data)))


def jpath_to_python(data):
    """
    The opposite of python_to_jpath: converts a representation used internally
    into JSON data suitable for, as an example, passing to simplejson.dumps.
    """
    if isinstance(data, (String, Boolean)):
        return data.get_value()
    elif isinstance(data, Number):
        if data.is_whole():
            return data.get_integer()
        else:
            return data.get_float()
    elif isinstance(data, Null):
        return None
    elif isinstance(data, List):
        return [jpath_to_python(v) for v in data.get_items()]
    elif isinstance(data, Map):
        return dict([(jpath_to_python(p.get_key()), jpath_to_python(p.get_value())) for p in data.get_pairs()])
    else:
        raise Exception("No jpath -> python encoding for " + str(type(data)))
