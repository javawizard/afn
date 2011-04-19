
"""
This module contains the datatypes used throughout the JPath engine to
represent actual JSON data.
"""

from sane_total_ordering import total_ordering
import math

class Item(object):
    """
    A JSON item. This represents a single value that can be passed around a
    query and stored in a collection. This class might be considered the
    JPath implementation equivalent of Python's object.
    
    All of the direct subclasses contain a set of functions that are used to
    access the underlying data. The query engine should depend /only/ on these
    functions, and not on any incidental data (and particularly not on any
    functions whose names start with an underscore). This allows instances of
    subclasses with optimized behavior to be passed into a query. For example,
    a database might be represented as one huge list, and storing such a list
    in memory would be impractical. Large documents stored in the database
    would also be impractical to represent in memory. This allows custom
    implementations to, for example, continue to store them on disk and
    consult indexes to retrieve the appropriate information. 
    """

class Identity(object):
    """
    JSON structures which have a notion of identity. Map, List, and Pair are
    currently the only subclasses of this class. Subclasses provide one
    method: same_as, which compares this object to see if it represents the
    exact same data as the specified object.
    """
    def same_as(self, other):
        """
        Returns true if the specified object represents the exact same item as
        this object. Subclasses should override this as necessary. The default
        implementation returns 'self is other'.
        """
        return self is other


@total_ordering
class Pair(Item, Identity):
    def __init__(self, key, value):
        self._key = key
        self._value = value
    
    def get_key(self):
        """
        Returns the key of this pair.
        """
        return self._key
    
    def get_value(self):
        """
        Returns the value of this pair.
        """
        return self._value
    
    def __hash__(self):
        return hash((self.get_key(), self.get_value()))
    
    def __eq__(self, other):
        if not isinstance(other, Pair):
            return NotImplemented
        return self.get_key() == other.get_key() and self.get_value() == other.get_value()
    
    def __lt__(self, other):
        if not isinstance(other, Pair):
            return NotImplemented
        this_key = self.get_key()
        other_key = other.get_key()
        if this_key == other_key:
            return self.get_value() < other.get_value()
        else:
            return this_key < other_key


@total_ordering
class Map(Item, Identity):
    # TODO: define the values that all the hash functions should return to
    # potentially allow looking up values in maps where the keys are the same
    # but are implemented by different subclasses. Also figure out if Python
    # even allows that.
    def __init__(self, pairs):
        """
        Creates a new map from the specified Python dictionary, whose keys are
        the keys in the map and whose values are Pair instances.
        """
        if not isinstance(map, (list, tuple)):
            raise Exception("Value passed into Map is not a list of pairs")
        # TODO: add an option to disable this check for increased performance
        for pair in pairs:
            if not isinstance(pair, Pair):
                raise Exception("Values passed into jpath.data.Map must be "
                        "Pair instances, not " + str(type(pair)))
        self._map = dict((pair.key, pair) for pair in pairs)
    
    def get_keys(self):
        """
        Returns a list of all keys present in this map.
        """
        return self._map.keys()
    
    def get_values(self):
        """
        Returns a list of all values present in this map.
        """
        # TODO: if this ends up being used a lot, consider caching a list of
        # values for a map alongside the map of keys to pairs
        return [pair.value for pair in self._map.values()]
    
    def get_pairs(self):
        """
        Returns a list of pairs, each one corresponding to an entry in this
        map.
        """
        return self._map.values()
    
    def get_key_count(self):
        """
        Returns the number of keys present in this map.
        """
        return len(self._map)
    
    def get_value(self, key):
        """
        Returns the value corresponding to a particular key in this map. If
        there is no entry for the specified key, this should return None.
        """
        pair = self._map.get(key, None)
        if pair:
            return pair.value
        return None
    
    def get_pair(self, key):
        """
        Returns the pair corresponding to the specified key.
        """
        return self._map.get(key, None)
    
    def __hash__(self):
        # Optimization for actual instances of Map
        if type(self) == Map:
            try:
                return self._cached_hash
            except AttributeError:
                self._cached_hash = hash(sorted(self.get_pairs()))
                return self._cached_hash
        return hash(sorted(self.get_pairs()))
    
    def __eq__(self, other):
        if not isinstance(other, Map):
            return NotImplemented
        # Optimization for actual instances of Map
        if type(self) == Map and type(other) == Map:
            return self._map == other._map
        return set(self.get_pairs()) == set(other.get_pairs())
    
    def __lt__(self, other):
        if not isinstance(other, Map):
            return NotImplemented
        return sorted(self.get_pairs()) < sorted(other.get_pairs())


@total_ordering
class List(Item, Identity):
    def __init__(self, data):
        if not isinstance(data, list):
            raise Exception("Value is not a list")
        self._data = data
    
    def get_item_range(self, start, end):
        """
        Returns a list of items from this list, starting at start, inclusive,
        and ending with end, exclusive, both of which are zero-based. If
        either of those indexes are out of range, they should be automatically
        constrained to be in range. If no items are within the specified
        range, the empty list should be returned.
        """
        return self._data[start:end]
    
    def get_items(self):
        """
        Returns a list of all items in this list.
        """
        # Is copying this really necessary? Ideally any code calling this
        # function wouldn't modify the result, but just to be safe (and
        # especially for compatibility with code outside of the JPath engine
        # that might use JPath objects directly), we're copying it for now.
        # TODO: check this out further, as this could slow stuff down a lot to
        # leave this copy operation in place.
        return self._data[:]
    
    def get_item_count(self):
        """
        Returns the number of items contained within this list.
        """
        return len(self._data)
    
    def __hash__(self):
        # Optimization for actual instances of List
        if type(self) == List:
            return hash(tuple(self._data))
        return hash(tuple(self.get_items()))
    
    def __eq__(self, other):
        if not isinstance(other, List):
            return NotImplemented
        # Optimization for actual instances of List
        if type(self) == List and type(other) == List:
            return self._data == other._data
        return self.get_items() == other.get_items()
    
    def __lt__(self, other):
        if not isinstance(other, List):
            return NotImplemented
        # Optimization for actual instances of List
        if type(self) == List and type(other) == List:
            return self._data < other._data
        return self.get_items() < other.get_items()


@total_ordering
class String(Item):
    def __init__(self, value):
        if not isinstance(value, basestring):
            raise Exception("The value of a string has to be either a str or "
                    "a unicode. If you want to supply your own custom "
                    "string-like object, subclass String and do it that way. ")
        self._value = value
    
    def get_value(self):
        """
        Returns the value of this string as a string or a unicode.
        """
        return self._value
    
    def __hash__(self):
        return hash(self._value)
    
    def __eq__(self, other):
        if not isinstance(other, String):
            return NotImplemented
        return self.get_value() == other.get_value()
    
    def __lt__(self, other):
        if not isinstance(other, String):
            return NotImplemented
        return self.get_value() < other.get_value()


@total_ordering
class Number(Item):
    def __init__(self, value):
        self._value = float(value)
    
    def get_float(self):
        """
        Returns the value of this number as a double.
        """
        return self._value
    
    def get_integer(self):
        """
        Returns the value of this number as a whole integer (or long).
        """
        return int(self._value)
    
    def is_whole(self):
        """
        Returns true if this number is a whole number, or false if this number
        has a fractional part.
        """
        return math.floor(self._value) == self._value
    
    def __hash__(self):
        return hash(self._value)
    
    def __eq__(self, other):
        if not isinstance(other, Number):
            return NotImplemented
        return self.get_float() == other.get_float()
    
    def __lt__(self, other):
        if not isinstance(other, Number):
            return NotImplemented
        return self.get_float() < other.get_float()


@total_ordering
class Boolean(Item):
    def __init__(self, value):
        if not isinstance(value, bool):
            raise Exception("Value is not a boolean")
        self._value = value
    
    def get_value(self):
        """
        Returns the value of this boolean as a Python boolean.
        """
        return self._value
    
    def __hash__(self):
        return hash(self._value)
    
    def __eq__(self, other):
        if not isinstance(other, Boolean):
            return NotImplemented
        return self.get_value() == other.get_value()
    
    def __lt__(self, other):
        if not isinstance(other, Boolean):
            return NotImplemented
        return self.get_value() < other.get_value()


@total_ordering
class Null(Item):
    def __hash__(self):
        return 0
    
    def __eq__(self, other):
        if not isinstance(other, Null):
            return NotImplemented
        return True # All nulls are equal
    
    def __lt__(self, other):
        if not isinstance(other, Null):
            return NotImplemented
        return False # All nulls are equal

