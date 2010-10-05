
from objectivejson import Serializer
serializer = Serializer()
serializable = serializer.serializable

@serializable
class Instance(object):
    pass

@serializable
class IntInstance(Instance):
    serial_info = [("value", int, False)]
    def __init__(self, value=0):
        self.value = value

@serializable
class BoolInstance(Instance):
    serial_info = [("value", bool, False)]
    def __init__(self, value=False):
        self.value = value

@serializable
class StringInstance(Instance):
    serial_info = [("value", str, False)]
    def __init__(self, value=None):
        if value is not None:
            self.value = value

@serializable
class MapInstance(Instance):
    serial_info = [("value", dict, False)]
    def __init__(self, value=None):
        if value is None:
            value = {}
        self.value = value

@serializable
class ListInstance(Instance):
    serial_info = [("value", list, False)]
    def __init__(self, value=None):
        if value is None:
            value = []
        self.value = value

@serializable
class NullInstance(Instance):
    serial_info = []

"""
@serializable
class Signature(object):
    pass

@serializable
class IntSignature(object):
    serial_info = []

@serializable
class BoolSignature(object):
    serial_info = []

@serializable
class StringSignature(object):
    serial_info = []

@serializable
class MapSignature(object):
    serial_info = [
        ("key_type", "signature", )
    ]

@serializable
class ListSignature(object):
    pass

@serializable
class NullSignature(object):
    pass
"""

@serializable
class Message(object):
    pass

@serializable
class RegisterInterface(Message):
    pass

@serializable
class RegisterFunction(Message):
    pass

@serializable
class CallFunction(Message):
    pass

@serializable
class RunFunction(Message):
    pass

