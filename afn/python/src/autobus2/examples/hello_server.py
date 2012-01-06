
from autobus2 import Bus, wait_for_interrupt

def main():
    with Bus() as bus:
        service = bus.create_service({"autobus.example": "hello_server"})
        def hi(text="world"):
            print "Saying hi to " + str(text)
            return "Hi, " + str(text) + "! How are you?"
        service.create_function("hi", hi)
        service.activate()
        
        wait_for_interrupt()
