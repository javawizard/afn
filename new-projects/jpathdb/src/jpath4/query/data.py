
from abc import ABCMeta as ABC, abstractmethod as abstract
from jpath4.query import utils, exceptions as e
from jpath4.sane_total_ordering import total_ordering
import json

def core_type(c):
    c.jpath_type = c
    return c


@total_ordering
class Item(object):
    """
    A JPath item. All JPath values are represented as instances of the various
    subclasses of this class. It contains methods corresponding to the core
    operations that can be performed on a JPath value.
    
    All Items are hashable. Items are not required to be read-only, but most
    are, and the behavior of an item used as a key in an Object is undefined
    if the item is changed for some reason.
    
    Certain subclasses of Item are known as core types. Such subclasses are
    decorated with the core_type decorator. It sets the class's jpath_type
    attribute to the class itself, which allows for determining the core type
    superclass of any given subclass of Item or any instance of such a
    subclass by checking the value of its jpath_type attribute.
    
    Certain methods defined in Item behave differently depending on what the
    core type of the instance on which said methods are called is. Those
    methods mention such behavior in their documentation.
    
    Probably the simplest way to describe what a core type is would be to say
    that it represents the JPath type of an item. For example, Object, List,
    Number, Boolean, String, and Null are all core types; this allows, for
    example, the various subclasses of Object to still behave the same.
    """
    __metaclass__ = ABC
    
    @abstract
    def get_children(self):
        """
        Returns a Sequence containing the children of this item. This is
        equivalent to the JPath expression *, i.e. a single asterisk.
        Subclasses of Item that do not have a notion of children (such as
        Number) should return an empty sequence from this method.
        """
    
    @abstract
    def get_pair_children(self):
        """
        Returns a Sequence containing the pair children of this item. This is
        equivalent to the JPath expression @*. Subclasses of Item that do not
        have a notion of pair children should return an empty sequence from
        this method.
        """
    
    @abstract
    def get_for_pattern(self, pattern):
        """
        Returns a Sequence containing the values matched by the given pattern,
        which will be passed in as an instance of Python's str or unicode
        classes. The JPath expression some-pattern is equivalent to calling
        get_for_pattern("some-pattern") on the context item.
        """
    
    @abstract
    def get_for_pair_pattern(self, pattern):
        """
        Returns a Sequence containing the values matched by the given pair
        pattern, which will be passed in as an instance of Python's str or
        unicode classes. The JPath expression @some-pattern is equivalent to
        calling get_for_pair_pattern("some-pattern") on the context item.
        """
    
    @abstract
    def get_for_indexer(self, value):
        """
        Returns a Sequence containing the values matched by the given indexer.
        The indexer will be passed in as a Sequence. This is equivalent to
        #some-expr in JPath.
        """
    
    @abstract
    def get_for_pair_indexer(self, value):
        """
        Returns a Sequence containing the values matched by the given pair
        indexer. The indexer will be passed in as a Sequence. This is
        equivalent to @#some-expr in JPath.
        """
    
    @abstract
    def equal(self, other):
        """
        Returns True if this Item is equal to the specified Item, False if it
        is not.
        
        This class provides an __eq__ method that checks to see if the
        specified value is of the same core type as this value. If they
        aren't, Item.__eq__ automatically returns false without even calling
        equal. Because of this, you don't need to perform any type checking
        in this method; when it's called, other will be guaranteed to be an
        instance of the core type of this value.
        
        Most core types implement this method, so subclasses of core type
        classes that themselves subclass from Item typically won't have to
        override this method.
        """
    
    @abstract
    def less_than(self, other):
        """
        Returns True if this Item is less than the specified Item, False if it
        is not.
        
        This method follows the same guidelines as the equal method;
        specifically, it will not be called unless other is of the same core
        type as self is. When comparing two Items with __lt__ and all of the
        other comparison operators that Item provides, if the items are not of
        the same core type, a decision will be made that consistently orders
        items of one core type before items of another core type. This
        decision is currently based on comparing the two core types with the
        less-than operator, which Python defines for normal classes to be
        arbitrary but consistent. The result is that the ordering of two Item
        instances of different core types is arbitrary but consistent across a
        particular Python invocation.
        
        As with the equal method, most core types provide an implementation of
        this method, so subclasses typically won't have to override it. 
        """
    
    @abstract
    def to_jpath(self):
        """
        Converts this value into a JPath expression that could be used to
        reconstruct a corresponding value of the same core type.
        """
    
    def to_string(self):
        # I decided to allow everything to be converted to a string and to
        # take its default JPath representation if another conversion is not
        # specified, so this is commented out for now.
        # raise e.TypeException("Trying to convert a value of type " +
        #         str(type(self)) + " to a string, but this type does not "
        #         "support converting itself into a string.")
        return self.to_jpath()
    
    def __eq__(self, other):
        if not isinstance(other, Item):
            return False
        if self.jpath_type != other.jpath_type:
            return False
        return self.equal(other)
    
    def __lt__(self, other):
        if not isinstance(other, Item):
            return NotImplemented
        our_type = self.jpath_type
        other_type = other.jpath_type
        if our_type != other_type: # Not the same JPath type, so we'll return
            # a comparison based on the JPath type to guarantee consistent but
            # arbitrary ordering among JPath types
            return our_type < other_type
        # Same JPath type, so we ask it to do the comparison.
        return self.less_than(other)
    
    @abstract
    def __hash__(self): pass


class IdentityItem(object):
    """
    Provides ordering operation implementations that assume that an item is
    only equal to its identity-wise self. This class is intended to be used
    for items where such behavior makes sense, such as most updates.
    """
    def equal(self, other):
        return self is other
    
    def less_than(self, other):
        return id(self) < id(other)
    
    def __hash__(self):
        return id(self)


class EmptyItem(object):
    """
    Provides implementations of get_for_pattern and family that return the
    empty sequence. This class is intended to be used as a mix-in for items
    where such behavior makes sense, such as most updates (although a lot of
    the update classes override get_for_pattern to provide information about
    the update).
    
    This class's implementation of get_for_indexer delegates to
    get_for_pattern if the value is a string; this can be used to override
    get_for_pattern if pattern behavior is desired.
    """
    def get_children(self): return EmptySequence()
    
    def get_pair_children(self): return EmptySequence()
    
    def get_for_pattern(self, pattern): return EmptySequence()
    
    def get_for_pair_pattern(self, pattern): return EmptySequence()
    
    def get_for_indexer(self, value):
        single = utils.try_single_instance(value, String)
        if single:
            return self.get_for_pattern(single.get_value())
        return EmptySequence()
    
    def get_for_pair_indexer(self, value):
        single = utils.try_single_instance(value, String)
        if single:
            return self.get_for_pair_pattern(single.get_value())
        return EmptySequence()


@total_ordering
class Sequence(object):
    __metaclass__ = ABC
    
    @abstract
    def get_item(self, index): pass
    
    @abstract
    def get_size(self): pass
    
    @abstract
    def is_synthetic(self): pass
    
    def to_jpath(self):
        if self.get_size() == 1:
            return self.get_item(0).to_jpath()
        return "(" + ", ".join(v.to_jpath() for v in self) + ")"
    
    def to_python_list(self):
        """
        Converts this sequence into a Python list. Subclasses can override
        this if they can provide a more efficient implementation.
        """
        return [self.get_item(i) for i in xrange(self.get_size)]
    
    def __iter__(self):
        """
        Returns an iterator over this sequence's items. Subclasses can
        override this if they can provide a more efficient implementation.
        """
        for i in xrange(self.get_size()):
            yield self.get_item(i)
    
    def __len__(self):
        return self.get_size()
        
    def __eq__(self, other):
        if not isinstance(other, Sequence):
            return False
        return self.to_python_list() == other.to_python_list()
    
    def __lt__(self, other):
        if not isinstance(other, Sequence):
            return NotImplemented
        return self.to_python_list() < other.to_python_list()


@core_type
class Number(Item):
    __metaclass__ = ABC
    
    @abstract
    def get_as_float(self): pass
    
    @abstract
    def get_as_int(self): pass
    
    def get_children(self):
        return EmptySequence()
    
    def get_pair_children(self):
        return EmptySequence()
    
    def get_for_pattern(self, pattern):
        return EmptySequence()
    
    def get_for_pair_pattern(self, pattern):
        return EmptySequence()
    
    def get_for_indexer(self, value):
        return EmptySequence()
    
    def get_for_pair_indexer(self, value):
        return EmptySequence()
    
    def equal(self, other):
        return other.get_as_float() == self.get_as_float()
    
    def less_than(self, other):
        return self.get_as_float() < other.get_as_float()
    
    def __hash__(self):
        return hash(self.get_as_float())
    
    def to_jpath(self):
        value = self.get_as_float()
        if int(value) == value: # value is a whole number
            return str(int(value))
        return str(value)


@core_type
class Boolean(Item):
    __metaclass__ = ABC
    
    @abstract
    def get_value(self): pass
    
    def get_children(self):
        return EmptySequence()
    
    def get_pair_children(self):
        return EmptySequence()
    
    def get_for_pattern(self, pattern):
        return EmptySequence()
    
    def get_for_pair_pattern(self, pattern):
        return EmptySequence()
    
    def get_for_indexer(self, value):
        return EmptySequence()
    
    def get_for_pair_indexer(self, value):
        return EmptySequence()
    
    def equal(self, other):
        return other.get_value() == self.get_value()
    
    def less_than(self, other):
        return self.get_value() < other.get_value()
    
    def __hash__(self):
        return hash(self.get_value())
    
    def to_jpath(self):
        return "true" if self.get_value() else "false"


@core_type
class Null(Item):
    __metaclass__ = ABC
    
    def get_children(self):
        return EmptySequence()
    
    def get_pair_children(self):
        return EmptySequence()
    
    def get_for_pattern(self, pattern):
        return EmptySequence()
    
    def get_for_pair_pattern(self, pattern):
        return EmptySequence()
    
    def get_for_indexer(self, value):
        return EmptySequence()
    
    def get_for_pair_indexer(self, value):
        return EmptySequence()
    
    def equal(self, other):
        return True
    
    def less_than(self, other):
        return False
    
    def __hash__(self):
        return 0
    
    def to_jpath(self):
        return "null"


@core_type
class String(Item):
    __metaclass__ = ABC
    
    @abstract
    def get_value(self): pass
    
    def get_children(self):
        return StandardSequence([StandardString(s) for s in self.get_value()])
    
    def get_pair_children(self):
        return EmptySequence()
    
    def get_for_pattern(self, pattern):
        return EmptySequence()
    
    def get_for_pair_pattern(self, pattern):
        return EmptySequence()
    
    def get_for_indexer(self, value):
        index = utils.get_single_instance(value, Number).get_as_int()
        this_value = self.get_value()
        if index < 0 or index >= len(this_value):
            return EmptySequence() # or raise an exception? Check on XQuery's behavior for this
        return StandardSequence([StandardString(this_value[index])])
    
    def get_for_pair_indexer(self, value):
        return EmptySequence()
    
    def equal(self, other):
        return other.get_value() == self.get_value()
    
    def less_than(self, other):
        return self.get_value() < other.get_value()
    
    def __hash__(self):
        return hash(self.get_value())
    
    def to_jpath(self):
        # This is sort of hackish; is it the best way to go about it?
        return json.dumps(self.get_value())
    
    def to_string(self):
        return self.get_value()


@core_type
class List(Item):
    """
    A list.
    
    get_items returns a Sequence of all items in this list.
    """
    __metaclass__ = ABC
    
    @abstract
    def get_size(self): pass
    
    @abstract
    # NOTE: This, unlike Object.get_value, seems not to be called if the index
    # in question is out of range. If this is true, I need to duly document it
    # so that people don't write unnecessary range checks.
    def get_item(self, index): pass
    
    @abstract
    def get_items(self): pass
    
    def to_python_list(self):
        """
        Converts this List into a Python list. Subclasses can override this
        if they have a more efficient implementation than simply iterating
        through all the items and building up a Python list.
        """
        return [self.get_item(i) for i in xrange(self.get_size())]
    
    def __iter__(self):
        """
        Returns an iterator over this list's values. Subclasses can override
        this if they can provide a more efficient implementation.
        """
        for i in xrange(self.get_size()):
            yield self.get_item(i)
    
    def get_children(self):
        return StandardSequence([self.get_item(i) for i in xrange(self.get_size())])
    
    def get_pair_children(self):
        return StandardSequence([])
    
    def get_for_pattern(self, pattern):
        return utils.flatten([self.get_item(i).get_for_pattern(pattern) for i in xrange(self.get_size())])
    
    def get_for_pair_pattern(self, pattern):
        return utils.flatten([self.get_item(i).get_for_pair_pattern(pattern) for i in xrange(self.get_size())])
    
    def get_for_indexer(self, value):
        if value.get_size() == 1:
            single = value.get_item(0)
            if isinstance(single, Number):
                number = single[0].get_as_int()
                if number >= 0 and number < self.get_size():
                    return StandardSequence([self.get_item(number)])
                else:
                    return StandardSequence([])
        return utils.flatten([self.get_item(i).get_for_indexer(value) for i in xrange(self.get_size())])
    
    def get_for_pair_indexer(self, value):
        return utils.flatten([self.get_item(i).get_for_pair_indexer(value) for i in xrange(self.get_size())])
    
    def equal(self, other):
        if self.get_size() != other.get_size():
            return False
        for i in xrange(self.get_size()):
            if not self.get_item(i).equal(other.get_item(i)):
                return False
        return True
    
    def less_than(self, other):
        return self.to_python_list() < other.to_python_list()
    
    def __hash__(self):
        return hash([self.get_item(i) for i in xrange(self.get_size())])
    
    def to_jpath(self):
        return "[" + ", ".join(v.to_jpath() for v in self) + "]"


@core_type
class Pair(Item):
    __metaclass__ = ABC
    
    @abstract
    def get_key(self):
        """
        Returns the key of this pair, as an instance of an Item subclass.
        """
    
    @abstract
    def get_value(self):
        """
        Returns the value of this pair, as an instance of an Item subclass.
        """
    
    def __iter__(self):
        """
        Returns an iterator that yields exactly two items: the key of this
        Pair and the value of this Pair. Subclasses can override this if they
        can provide a more efficient implementation.
        
        For those of you that might be wondering why Pairs are even iterable,
        the reason is so that they can be unpacked in Python expressions; this
        allows for iterating over the keys and values in an Object, for
        example, by doing something like this:
        
        for key, value in some_object:
            ...
        """
        yield self.get_key()
        yield self.get_value()
    
    def get_children(self):
        return StandardSequence([])
    
    def get_pair_children(self):
        return StandardSequence([])
    
    def get_for_pattern(self, pattern):
        if pattern == "key" or pattern == "name":
            return StandardSequence([self.get_key()])
        if pattern == "value":
            return StandardSequence([self.get_value()])
        return StandardSequence([])
    
    def get_for_pair_pattern(self, pattern):
        return StandardSequence([])
    
    def get_for_indexer(self, value):
        if value.get_size() == 1:
            if isinstance(value.get_item(0), String):
                return self.get_for_pattern(value.get_item(0).get_value())
        return StandardSequence([])
    
    def get_for_pair_indexer(self, value):
        return StandardSequence([])
    
    def equal(self, other):
        if not isinstance(other, Pair):
            return False
        return self.get_key().equal(other.get_key()) and self.get_value().equal(other.get_value())
    
    def less_than(self, other):
        self_key = self.get_key()
        other_key = other.get_key()
        if self_key < other_key:
            return True
        if other_key < self_key:
            return False
        # Keys are equal, so check values
        return self.get_value() < other.get_value()
    
    def __hash__(self):
        return hash(self.get_key()) ^ hash(self.get_value())
    
    def to_jpath(self):
        return self.get_key().to_jpath() + ": " + self.get_value().to_jpath()


@core_type
class Object(Item):
    """
    A JSON object. These are normally created with a JPath/JSON expression of
    the form {"a": "b", "c": "d"}. This class allows
    """
    __metaclass__ = ABC
    
    @abstract
    def get_value(self, key):
        """
        Returns the value contained in this object for the specified key, or
        None if there is no such key. The key is a JPath Item instance, and
        it can be an instance of an Item subclass other than String. (JSON
        itself requires object keys to be strings, but JPath places no such
        limitation on objects; this is important to remember when writing an
        Object subclass that does place such a limitation.)
        """
    
    @abstract
    def get_pair(self, key):
        """
        Returns the pair corresponding to the value contained in this object
        for the specified key, or None if there is no such key. The key is a
        JPath Item instance as described in the doc for get_value.
        """
    
    @abstract
    def get_values(self):
        """
        Returns a Sequence containing all values present in this object.
        """
    
    @abstract
    def get_pairs(self):
        """
        Returns a Sequence containing all pairs present in this object.
        """
    
    @abstract
    def get_size(self):
        """
        Returns the size of this object, as a normal Python int or long.
        """
    
    def to_python_dict(self):
        """
        Same as List.to_python_list, but for Objects, and converts them to a
        Python dict.
        """
        return dict([(p.get_key(), p.get_value()) for p in self.get_pairs()])
    
    def __iter__(self):
        """
        Returns an iterator over this object's pairs. Subclasses can override
        this if they can provide a more efficient implementation.
        """
        pairs = self.get_pairs()
        for v in pairs: # pairs is a sequence, and sequences are also
            # iterable. TODO: consider changing this to return iter(pairs).
            yield v
    
    def get_children(self):
        return self.get_values()
    
    def get_pair_children(self):
        return self.get_pairs()
    
    def get_for_pattern(self, pattern):
        value = self.get_value(StandardString(pattern))
        if value is None:
            return EmptySequence()
        return utils.singleton(value)
    
    def get_for_pair_pattern(self, pattern):
        value = self.get_pair(StandardString(pattern))
        if value is None:
            return EmptySequence()
        return utils.singleton(value)
    
    def get_for_indexer(self, value):
        result = self.get_value(utils.get_single(value))
        if result is None:
            return EmptySequence()
        return utils.singleton(result)
    
    def get_for_pair_indexer(self, value):
        result = self.get_pair(utils.get_single(value))
        if result is None:
            return EmptySequence()
        return utils.singleton(result)
    
    def equal(self, other):
        return self.to_python_dict() == other.to_python_dict()
    
    def less_than(self, other):
        return self.to_python_dict() < other.to_python_dict()
    
    def __hash__(self):
        return hash(self.to_python_dict())
    
    def to_jpath(self):
        return "{" + ", ".join(v.to_jpath() for v in self) + "}"


class EmptySequence(Sequence):
    def get_item(self, index):
        raise IndexError
    
    def get_size(self):
        return 0
    
    def is_synthetic(self):
        return False
    
    def __repr__(self):
        return "EmptySequence()"
    
    def to_python_list(self):
        return []


class StandardSequence(Sequence):
    def __init__(self, values):
        if not isinstance(values, (list, tuple)):
            raise Exception("values is %s" % repr(values))
        self.values = values
    
    def get_item(self, index):
        return self.values[index]
    
    def get_size(self):
        return len(self.values)
    
    def is_synthetic(self):
        return False
    
    def __repr__(self):
        return "StandardSequence(%s)" % repr(self.values)
    
    def to_python_list(self):
        return self.values
    
    def iterator(self):
        return self.values.__iter__()


class StandardString(String):
    def __init__(self, value):
        self.value = value
    
    def get_value(self):
        return self.value
    
    def __repr__(self):
        return "StandardString(%s)" % repr(self.value)


class StandardNumber(Number):
    def __init__(self, value):
        self.value = value
    
    def get_as_float(self):
        return float(self.value)
    
    def get_as_int(self):
        return int(self.value)
    
    def __repr__(self):
        return "StandardNumber(%s)" % repr(self.value)


class StandardBoolean(Boolean):
    def __init__(self, value):
        self.value = value
    
    def get_value(self):
        return self.value
    
    def __repr__(self):
        return "StandardBoolean(%s)" % self.value


class StandardNull(Null):
    def __repr__(self):
        return "StandardNull()"


class StandardList(List):
    def __init__(self, list):
        self.list = list
    
    def get_size(self):
        return len(self.list)
    
    def get_item(self, index):
        return self.list[index]
    
    def get_items(self):
        return StandardSequence(self.list)
    
    def __repr__(self):
        return "StandardList(%s)" % repr(self.list)
    
    def to_python_list(self):
        return self.list


class StandardPair(Pair):
    def __init__(self, key, value):
        self.key = key
        self.value = value
    
    def get_key(self):
        return self.key
    
    def get_value(self):
        return self.value
    
    def __repr__(self):
        return "StandardPair(%s, %s)" % (repr(self.key), repr(self.value))


class StandardObject(Object):
    def __init__(self, value):
        self.pair_dict = {}
        self.value_dict = {}
        if not isinstance(value, (Sequence, list, tuple)):
            raise Exception(type(value))
        for p in value:
            if not isinstance(p, Pair):
                raise Exception(type(p))
            k = p.get_key()
            self.pair_dict[k] = p
            self.value_dict[k] = p.get_value()
    
    def get_value(self, key):
        return self.value_dict.get(key, None)
    
    def get_values(self):
        return StandardSequence(self.value_dict.values())
    
    def get_pair(self, key):
        return self.pair_dict.get(key, None)
    
    def get_pairs(self):
        return StandardSequence(self.pair_dict.values())
    
    def get_size(self):
        return len(self.pair_dict)
    
    def __repr__(self):
        return "StandardObject(%s)" % self.get_pairs().to_python_list()
    
    def to_python_dict(self):
        return self.value_dict





































