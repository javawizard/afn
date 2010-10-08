
from libautobus import AutobusConnection

server = AutobusConnection()
server.connect()

interface = server["example"]
print interface.say_hello("Alex")

server.shutdown()
