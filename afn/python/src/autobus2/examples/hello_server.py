
from autobus2 import Bus, wait_for_interrupt
from autobus2.providers import PyServiceProvider

class HelloService(PyServiceProvider):
    def hi(self, text="world"):
        print "Saying hi to " + str(text)
        return "Hi, " + str(text) + "! How are you?"


def main():
    with Bus() as bus:
        bus.create_service({"autobus.example": "hello_server"}, HelloService())
        wait_for_interrupt()
