
from libautobus import AutobusConnection

class ExampleInterface(object):
    def say_hello(self, name):
        return "Hello, " + name + ". How are you?"

for i in range(3):
    server = AutobusConnection()
    server.add_interface("afntest.autobus3." + str(i), ExampleInterface(), None)
    server.connect()

