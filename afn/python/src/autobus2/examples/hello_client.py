from autobus2 import Bus
import sys
from time import sleep

def main():
    with Bus() as bus:
        sleep(1)
        with bus.connect_to({"autobus.example": "hello_server"}) as connection:
            connection.wait_for_connect()
            print connection["hi"]("great big round world")
