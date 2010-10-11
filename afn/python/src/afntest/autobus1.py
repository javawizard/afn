
from libautobus import AutobusConnection
from time import sleep

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
        print "We're about to say hi to " + name
        return "Hello, " + name + ". How are you?"

server = AutobusConnection()
server.add_interface("example", ExampleInterface())
server.connect()


# That's it for the actual autobus code. The rest of the code is a hack to get
# around python not letting the application die with ctrl+c.
try:
    while True:
        sleep(1)
except KeyboardInterrupt:
    print "Shutting down"
    server.shutdown()
