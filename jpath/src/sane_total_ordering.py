
import unittest

def total_ordering(cls):
    """Class decorator that fills in missing ordering methods
    
    It will also fill in __eq__ or __ne__ if the other one is present.
    """
    predefined = set(dir(cls))
    if not set(["__eq__", "__ne__"]) & predefined:
        raise ValueError("Must define either __eq__ or __ne__")
    if "__lt__" in predefined:
        def __ge__(self, other):
            lt = self.__lt__(other)
            if lt is NotImplemented:
                return NotImplemented
            return not lt
        def __le__(self, other):
            lt = self.__lt__(other)
            eq = self.__eq__(other)
            if lt is NotImplemented or eq is NotImplemented:
                return NotImplemented
            return lt or eq
        def __gt__(self, other):
            le = self.__le__(other)
            if le is NotImplemented:
                return NotImplemented
            return not le
        ops = [__ge__, __le__, __gt__]
    elif "__le__" in predefined:
        def __gt__(self, other):
            le = self.__le__(other)
            if le is NotImplemented:
                return NotImplemented
            return not le
        def __lt__(self, other):
            le = self.__le__(other)
            ne = self.__ne__(other)
            if le is NotImplemented or ne is NotImplemented:
                return NotImplemented
            return le and ne
        def __ge__(self, other):
            lt = self.__lt__(other)
            if lt is NotImplemented:
                return NotImplemented
            return not lt
        ops = [__gt__, __lt__, __ge__]
    elif "__gt__" in predefined:
        def __le__(self, other):
            gt = self.__gt__(other)
            if gt is NotImplemented:
                return NotImplemented
            return not gt
        def __ge__(self, other):
            gt = self.__gt__(other)
            eq = self.__eq__(other)
            if gt is NotImplemented or eq is NotImplemented:
                return NotImplemented
            return gt or eq
        def __lt__(self, other):
            ge = self.__ge__(other)
            if ge is NotImplemented:
                return NotImplemented
            return not ge
        ops = [__le__, __ge__, __lt__]
    elif "__ge__" in predefined:
        def __lt__(self, other):
            ge = self.__ge__(other)
            if ge is NotImplemented:
                return NotImplemented
            return not ge
        def __gt__(self, other):
            ge = self.__ge__(other)
            ne = self.__ne__(other)
            if ge is NotImplemented or ne is NotImplemented:
                return NotImplemented
            return ge and ne
        def __le__(self, other):
            gt = self.__gt__(other)
            if gt is NotImplemented:
                return NotImplemented
            return not gt
        ops = [__lt__, __gt__, __le__]
    else:
        raise ValueError("Must define one of __lt__, __le__, __gt__, __ge__")
    # Define __eq__ and __ne__ in terms of the other one if they don't exist
    if "__eq__" in predefined:
        def __ne__(self, other):
            eq = self.__eq__(other)
            if eq is NotImplemented:
                return NotImplemented
            return not eq
        ops.append(__ne__)
    elif "__ne__" in predefined:
        def __eq__(self, other):
            ne = self.__ne__(other)
            if ne is NotImplemented:
                return NotImplemented
            return not ne
        ops.append(__eq__)
    else:
        raise ValueError("Must define either __eq__ or __ne__")
    for op in ops:
        if op.__name__ not in predefined:
            # Copy docs from the corresponding functions on int
            op.__doc__ = getattr(int, op.__name__).__doc__
            setattr(cls, op.__name__, op)
    return cls



# One class for each of the four operators that we actually define
# One class for each of __eq__ and __ne__ being defined
# One class for A, B, and C, where A and C compare only among themselves but
#         B can compare itself with A as well
# One test for each of the six operators: >, <, >=, <=, ==, !=
# Test each operator with 6 op 2, 2 op 6, and 6 op 6
# Test each of the above combinations with A op A, A op B, B op A, B op B, and
#        C op C functioning as expected above, and A op C, B op C, C op A, and
#        C op B having undefined results but not throwing any exceptions

