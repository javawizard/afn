
from objectivejson import Serializer
serial = Serializer()

@serial.serializable("example")
class Example(object):
    serial_info = [
        ["first", int, True],
        ["second", (int, str), False]
    ]

e = Example()
e.first = 5
print serial.serialize(e)
e.second = "hello"
print serial.serialize(e)
e.second = 8
print serial.serialize(e)
del e.first
print serial.serialize(e)
e.first = None
print serial.serialize(e)
# This one should raise an exception
e.second = None
print serial.serialize(e)
print "It didn't throw an exception, which it should have"
