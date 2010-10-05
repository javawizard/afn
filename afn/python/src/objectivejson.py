
from functools import partial
import json

__doc__ = """\
This module implements a sort of serialization mechanism. It uses JSON as the
underlying encoding. It requires classes that are to be encoded to be decorated
with @serializable, which requires some information about which fields and
stuff can be serialized and what types they map to. The idea is that the
serialization scheme provided by this module is cross-language so that objects
serialized in Python could, for example, be deserialized in Java and vice
versa. To that end, the serializer is essentially statically-typed, although it
takes care of enforcing type constraints for you so you don't have to do it
yourself.

This module does not handle reference cycles. If you have any, you'll put the
serializer into an infinite loop. This doesn't pose any vulnerabilites to the
deserializer or anything, since the protocol itself doesn't support this.

Subclasses of built-in Python types (such as int or str) must be hashable for
the serializer to work correctly. They will generally get deserialized to their
Python built-in equivalent.

Classes have to be registered with a serializer instance before instances of
them can be serialized or deserialized. This is so that the serializer can use
language-independent names for objects.

Currently, this module only works properly with new-style classes.
"""

_NoValue = object()

class SerializerException(Exception):
    """
    Instances of this type or instances of subclasses thereof are raised when
    serialization problems happen.
    """

class NullException(SerializerException):
    """
    Raised when an object being serialized or deserialized contains a field
    that is null when the class's serial_info specified that the field cannot
    be null.
    """
    pass

class TypeMismatchException(SerializerException):
    """
    Raised when an object being serialized or deserialized contains a field
    whose value is not one of the allowed types for that field.
    """
    pass

class Serializer(object):
    """
    The main class in the serialization module. Instances of this class are
    used for serialization and deserialization. Once an instance has been
    configured, it is thread-safe for concurrent serialization and
    deserialization.
    """
    def __init__(self):
        self.types_to_names = {}
        self.names_to_types = {}
    
    def serializable(self, name, target_class=None):
        """
        Registers a class under the specified name for use with this
        serializer. The target class is returned. If the target class is not 
        specified, this function returns a function that can be passed the 
        target class with the same effect as calling this function itself,
        which allows this function to be used as a decorator thus:
        
        @some_serializer.serializable("the_class_name")
        class SomeClass(object):
            ...
        
        This is the reason this function returns the target class.
        Additionally, a class can be specified in place of the name, without
        specifying any value for target_class, which will result in this
        function acting as if it had been called with the specified class's
        name and the specified class passed as arguments. This allows this
        function to be used as a more compact decorator when the name is the
        same as the class's name thus:
        
        @some_serializer.serializable
        class SomeClass(object):
            ...
        
        which would register the class under the name "SomeClass".
        
        The target class should have an attribute named serial_info. This
        function will raise an exception if the attribute is missing or not
        well-formed. The attribute should be a list of sequences. Each sequence
        should have three items: the name of a field on the object that is to
        be serialized, a type or string or a sequence of types and strings representing
        the types that can be stored into the specified field, and a boolean
        indicating whether or not the field can be None. The second list, the
        one that specifies types and strings, consists of types for the
        primitive types (such as int, str, float, etc.) and strings for any
        type registered as serializable. Types specified thus as names are not
        checked for validity in case they have not yet been registered as
        serializable.
        """
        if isinstance(name, type):
            return self.serializable(name.__name__, name)
        if target_class is None:
            return partial(self.serializable, name)
        self.types_to_names[target_class] = name
        self.names_to_types[name] = target_class
        serial_info = target_class.serial_info
        serial_info_map = {}
        target_class.serial_info_map = serial_info_map
        for name, type_list, nullable in serial_info:
            if isinstance(type_list, (type, str)): 
                type_list = tuple([type_list])
            elif isinstance(type_list, list):
                type_list = tuple(type_list)
            serial_info_map[name] = type_list, nullable
        return target_class
    
    def serialize(self, serial_object):
        representation = self._serialize_to_object(serial_object)
        return json.dumps(representation, separators=(",",":"))
    
    def _serialize_to_object(self, serial_object):
        if isinstance(serial_object, (int, long, float, str, dict, type(None))):
            return serial_object
        if isinstance(serial_object, (list, tuple)):
            return [None, serial_object]
        # Not a primitive type, so we'll serialize it as an object.
        object_type = type(serial_object)
        info = object_type.serial_info_map
        result_map = {}
        result_list = [self.types_to_names[object_type], result_map]
        for name in info:
            type_list, nullable = info[name]
            try:
                value = getattr(serial_object, name)
            except:
                value = _NoValue
            if (not nullable) and value is None:
                raise NullException("Instance of " + str(object_type) + 
                                    ", attribute " + name)
            if value is not _NoValue:
                required_types = self._get_required_types(type_list)
                if not isinstance(value, required_types + (type(None),)):
                    raise TypeMismatchException("Expected object of type " + 
                                str(list(required_types)) + " on instance of "
                                + str(object_type) + " but got object of type "
                                + str(type(value)) + " instead")
                result_map[name] = self._serialize_to_object(value)
        return result_list
    
    def _get_required_types(self, type_list):
        return tuple((self.names_to_types[type_name] if 
                     isinstance(type_name, str) else type_name) for type_name
                     in type_list)
    
    def deserialize(self, text):
        model = json.loads(text)
        return self._deserialize_from_model(model)
    
    def _deserialize_from_model(self, model):
        if isinstance(model, (int, long, float, str, dict, type(None))):
            return model
        if isinstance(model, unicode):
            return str(model) # Need to add real support for unicode in the
            # future, maybe consider coercing str and unicode as specified
            # on an attribute's required values or something
        if not isinstance(model, list):
            raise SerializerException("Invalid instance type " + 
                                      str(type(model)) + " received")
        if model[0] is None: # Encoded list
            return model[1]
        # Encoded instance
        object_type = self.names_to_types[model[0]]
        info = object_type.serial_info_map
        result = object_type()
        object_map = model[1]
        for name in info:
            type_list, nullable = info[name]
            if name in object_map:
                setattr(result, name, 
                        self._deserialize_from_model(object_map[name]))
            try:
                value = getattr(result, name)
            except:
                value = _NoValue
            if (not nullable) and (value is None or value is _NoValue):
                raise NullException("Instance of " + str(object_type) + 
                                    ", attribute " + name)
            if value is not _NoValue:
                required_types = self._get_required_types(type_list)
                if not isinstance(value, required_types + (type(None),)):
                    raise TypeMismatchException("Expected object of type " + 
                                str(list(required_types)) + " on instance of "
                                + str(object_type) + " but got object of type "
                                + str(type(value)) + " instead")
        return result


















