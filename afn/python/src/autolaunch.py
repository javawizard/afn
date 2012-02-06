
from autobus2 import Bus

class Service(object):
    pass

def main():
    with Bus() as bus:
        bus.create_service({"type": "autolaunch"}, Service())