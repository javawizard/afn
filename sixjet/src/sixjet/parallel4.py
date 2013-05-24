
from parallel import Parallel
from autobus2 import Bus
from time import sleep
import time

DATA_A = 0x01
# The data pin for relay bank B
DATA_B = 0x02
# The clock pin. Setting this high causes the values currently on DATA_A and
# DATA_B to be shifted into the shift registers controlling relay banks A and
# B, respectively.
CLOCK = 0x08
# The strobe pin. Setting this high causes the values written to the device
# to be actually sent to the relays.
STROBE = 0x10

class ParallelSink(object):
    def __init__(self, server,
                 state_names=[[1, 2, 3, 4, 5, 6, 7, 8], [9, 10, 11, 12, 13, 14, 15, 16]],
                 data_pins=[DATA_A, DATA_B], strobe_pin=STROBE, clock_pin=CLOCK):
        self.server = server
        self.state_names = state_names
        self.flat_state_names = [name for l in state_names for name in l]
        self.data_pins = data_pins
        self.strobe_pin = strobe_pin
        self.clock_pin = clock_pin
        self.port = Parallel()
        self.write_function = self.port.setData
        self.service_dict = {}
    
    def receive_change(self, connection, info, name, old, new):
        if new is None:
            if name in self.service_dict:
                del self.service_dict[name]
        else:
            self.service_dict[name] = new
        self.compute_and_write()
    
    def compute_and_write(self):
        print "Before flush: %s" % time.ctime()
        state_dict = {}
        for states in self.service_dict.values():
            for k, v in states.iteritems():
                state_dict[k] = max(state_dict.get(k, 0), v)
        self.write_dict(state_dict)
        print "After flush: %s" % time.ctime()
    
    def set_parallel_data(self, data):
        """
        Sets the parallel port's data pins to the specified state, which should be
        a number from 0 to 255, then waits a bit.
        """
        self.write_function(data)
        sleep(0.0005) # 500 microseconds; increase if needed
    
    def write_dict(self, state_dict):
        self.write([[state_dict.get(name, 0) for name in group] for group in self.state_names])
    
    def write(self, states):
        self.write_actual(states)
        self.write_actual(states)
    
    def write_actual(self, states):
        """ 
        Writes the jet states stored in jet_states to the parallel port.
        """
        self.set_parallel_data(0)
        for current_bits in zip(*states):
            values = 0
            # current_bits will be a tuple, one item for each pin we are to
            # write, which match up with self.data_pins.
            for bit, pin in zip(current_bits, self.data_pins):
                if bit:
                    values |= pin
            self.set_parallel_data(values)
            # Do it an extra time just to see if it helps some issues I've been
            # seeing with data occasionally getting clocked in wrong
            self.set_parallel_data(values)
            self.set_parallel_data(values | self.clock_pin)
            self.set_parallel_data(values)
        self.set_parallel_data(self.strobe_pin)
        self.set_parallel_data(0)


def receive_change():

def main():
    with Bus() as bus:
        sink = ParallelSink()
        proxy = bus.get_service_proxy({"sixjet.provides": "states"}, multiple=True)
        proxy.watch_object("sixjet.states", sink.receive_change)
        bus.wait_for_interrupt()







