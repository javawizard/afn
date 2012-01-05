
from autobus2 import Bus, wait_for_interrupt
from autobus2.landfill.listeners import print_service_listener

def main():
    with Bus() as bus:
        print "Listening on port " + str(bus.port)
        bus.add_service_listener(print_service_listener, initial=True)
        service = bus.create_service({"autobus.example": "hello_server"}, active=False)
        print "Service id " + service.id
        def hi(text="world"):
            print "Saying hi to " + str(text)
            return "Hi, " + str(text) + "! How are you?"
        service.create_function("hi", hi)
        service.activate()
        
        wait_for_interrupt()
