
from afn.utils import singleton

ANY = singleton.Singleton("autobus2.filter.ANY")
NOT_PRESENT = singleton.Singleton("autobus2.filter.NOT_PRESENT")

def filter_matches(info, filter):
    if filter is None:
        return True
    for k, v in filter.items():
        if v is ANY:
            if k not in info:
                return False
        elif v is NOT_PRESENT:
            if k in info:
                return False
        else:
            if info[k] != v:
                return False
    return True