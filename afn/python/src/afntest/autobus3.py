
from libautobus import AutobusConnection

class ExampleInterface(object):
    def say_hello(self, name):
        return "Hello, " + name + ". How are you?"

server = AutobusConnection()
server.add_interface("example", ExampleInterface(), None)
server.connect()

