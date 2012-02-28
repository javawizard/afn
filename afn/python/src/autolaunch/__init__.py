
from autobus2 import Bus
import multiprocessing as mp

class Service(object):
    pass

def main():
    with Bus() as bus:
        bus.create_service({"type": "autolaunch"}, from_py_object=Service())
        