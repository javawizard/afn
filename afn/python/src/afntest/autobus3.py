
from libautobus import AutobusConnection
from os import getpid

class ExampleInterface(object):
    def say_hello(self, name):
        return "Hello, " + name + ". How are you?"

for i in range(10):
    server = AutobusConnection()
    server.add_interface("afntest.autobus3." + str(i), ExampleInterface())
    server.connect()

print "Started up. pid is " + str(getpid()) + " which you can use to kill"
print "this once you're done with it."