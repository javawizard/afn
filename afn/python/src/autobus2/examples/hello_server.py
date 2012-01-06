
from autobus2 import Bus, wait_for_interrupt

class HelloService(object):
    def hi(self, text="world"):
        print "Saying hi to " + str(text)
        return "Hi, " + str(text) + "! How are you?"


def main():
    with Bus() as bus:
        bus.create_service({"autobus.example": "hello_server"}, from_py_object=HelloService())
        wait_for_interrupt()
