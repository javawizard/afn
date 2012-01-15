
from autobus2 import Bus, wait_for_interrupt

class Service(object):
    def set(self, new_state):
        state_object.set_value(new_state)
        
def main():
    global state_object
    with Bus() as bus:
        service = bus.create_service({"type": "flipflop"}, active=False, use_py_object=Service())
        state_object = service.create_object("state", 1, "The current state.")
        service.activate()
        wait_for_interrupt()
