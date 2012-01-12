
import autobus2
import time

def main():
    with autobus2.Bus() as bus:
        service = bus.create_service({"autobus.example": "testobjectserver1"})
        object = service.create_object("test", time.time())
        service.activate()
        while True:
            try:
                time.sleep(3)
                object.set_value(time.time())
            except KeyboardInterrupt:
                return
