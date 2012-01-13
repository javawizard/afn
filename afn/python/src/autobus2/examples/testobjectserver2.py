
from autobus2 import Bus
import time

def main():
    with Bus() as bus:
        service = bus.create_service({"autobus.example": "testobjectserver2"})
        object = service.create_object("test", time.ctime(time.time()))
        service.activate()
        try:
            while True:
                time.sleep(5)
                object.set_value(time.ctime(time.time()))
        except KeyboardInterrupt:
            pass