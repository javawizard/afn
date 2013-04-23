
import json
from bindings import bind

def json_to_bindable(value):
    if isinstance(value, dict):
        d = bind.PyDict()
        for k, v in value.items():
            d[k] = json_to_bindable(v)
        return d
    if isinstance(value, list):
        l = bind.PyList()
        for v in value:
            l.append(json_to_bindable(v))
        return l
    return value


def bindable_to_json(value):
    if isinstance(value, bind.PyDict):
        d = {}
        for k, v in value.items():
            d[k] = bindable_to_json(v)
        return d
    if isinstance(value, bind.PyList):
        l = []
        for v in value:
            l.append(bindable_to_json(v))
        return l
    if isinstance(value, bind.Bindable):
        raise Exception("Unknown bindable type")
    return value
