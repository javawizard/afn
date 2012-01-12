
import autobus2
from time import sleep

def changed(*args):
    print "CHANGED: " + str(args)

def main():
    with autobus2.Bus() as bus:
        sleep(1)
        with bus.connect_to({"autobus.example": "testobjectserver1"}) as connection:
            connection.wait_for_connect()
            connection.watch_object("test", changed)
            autobus2.wait_for_interrupt()
