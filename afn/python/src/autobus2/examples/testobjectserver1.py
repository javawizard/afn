
import autobus2

def main():
    with autobus2.Bus() as bus:
        service = bus.create_service({"autobus.example": "testobjectserver1"})
        object = service.create_object("test", "hello")
        service.activate()
        autobus2.wait_for_interrupt()
