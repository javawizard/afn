
from autobus2 import Bus, wait_for_interrupt

def changed(*args):
    print "CHANGED: " + str(args)

def main():
    with Bus() as bus:
        with bus.get_service_proxy({"autobus.example": "testobjectserver2"},
                multiple=True) as proxy:
            proxy.watch_object("test", changed)
            wait_for_interrupt()
