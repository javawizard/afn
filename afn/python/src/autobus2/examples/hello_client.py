from autobus2 import Bus
import sys
from time import sleep

def main():
    with Bus() as bus:
        with bus.get_service_proxy({"autobus.example": "hello_server"}) as service:
            service.wait_for_bind(timeout=2)
            print service["hi"]("great big round world")
