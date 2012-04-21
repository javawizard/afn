
from jpath.query import data as d

def jpath_to_json(value, strict=False):
    """
    Converts the specified JPath data into Python objects/lists/numbers/etc
    representing the data. The resulting data is suitable for passing into
    Python's json.dumps.
    
    If strict is true, object keys are required to be strings, as JSON
    mandates; a non-string key will result in an exception. If strict is
    false, such keys will be allowed, but the resulting object might not be
    passable to json.dumps without causing an exception during the call.
    """
    if isinstance(value, d.Boolean):
        return value.get_value()
    if isinstance(value, d.Null):
        return None
    if isinstance(value, d.Number):
        return value.get_as_float()
    if isinstance(value, d.String):
        return value.get_value()
    if isinstance(value, d.Object):
        result = {}
        for k, v in value:
            k = jpath_to_json(k)
            if strict and not isinstance(k, d.String):
                raise Exception("Key " + str(k) + " of an object was not a "
                        "string, but strict formatting was requested. JSON "
                        "object keys must always be strings when strict "
                        "formatting is on.")
            result[k] = jpath_to_json(v)
        return result
    if isinstance(value, d.List):
        return [jpath_to_json(v) for v in value]
    raise TypeError("Can't convert values of type " + str(type(value))
            + " to json")


def json_to_jpath(value):
    if value is None:
        return d.StandardNull()
    if isinstance(value, bool):
        return d.StandardBoolean(value)
    if isinstance(value, (int, float, long)):
        return d.StandardNumber(value)
    if isinstance(value, (str, unicode)):
        return d.StandardString(value)
    if isinstance(value, (list, tuple)):
        return d.StandardList([json_to_jpath(v) for v in value])
    if isinstance(value, dict):
        return d.StandardObject([
                    d.StandardPair(json_to_jpath(k), json_to_jpath(v))
                    for (k, v) in value.items()
                ])
    raise TypeError("Can't convert values of type " + str(type(value))
            + " to jpath data")



































