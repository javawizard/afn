
from libautobus import AutobusConnection

class Interface(object):
    def set(self, new_state):
        state_object.set(new_state)
        
def main():
    global state_object
    bus = AutobusConnection()
    bus.add_interface("flipflop", Interface())
    state_object = bus.add_object("flipflop", "state", "The current state.", 1)
    bus.start_connecting()
    bus.interrupt_loop()
