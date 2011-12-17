
from autobus2 import Bus, wait_for_interrupt

with Bus() as bus:
    print "Listening on port " + str(bus.port)
    service = bus.create_service({"autobus.example": "hello_server"})
    print "Service id " + service.id
    def hi(text="hello world"):
        print text
    service.create_function("hi", hi)
    
    wait_for_interrupt()
