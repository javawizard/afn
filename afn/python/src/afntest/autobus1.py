
from libautobus import AutobusConnection

class ExampleInterface(object):
    """
    An example interface. It doesn't do much.
    
    Basically, all it allows you to do is say hello.
    """
    def say_hello(self, name):
        """
        Formats a "hello world"-style message greeting the specified person by
        name and returns it. The argument must be a string or an exception will
        be raised.
        """
        return "Hello, " + name + ". How are you?"

server = AutobusConnection()
server.add_interface("example", ExampleInterface(), None)
server.connect()

