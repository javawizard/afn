
from parallel import Parallel
from autobus2 import Bus
from time import sleep
import time
from sixjet.sink4 import Sink

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

class ParallelSink(Sink):
    def __init__(self,
                 state_names=[[1, 2, 3, 4, 5, 6, 7, 8], [9, 10, 11, 12, 13, 14, 15, 16]],
                 data_pins=[DATA_A, DATA_B], strobe_pin=STROBE, clock_pin=CLOCK):
        self.state_names = state_names
        self.flat_state_names = [name for l in state_names for name in l]
        self.data_pins = data_pins
        self.strobe_pin = strobe_pin
        self.clock_pin = clock_pin
        self.port = Parallel()
        self.write_function = self.port.setData
    
    def set_parallel_data(self, data):
        """
        Sets the parallel port's data pins to the specified state, which should be
        a number from 0 to 255, then waits a bit.
        """
        self.write_function(data)
        sleep(0.0005) # 500 microseconds; increase if needed
    
    def write(self, state_dict):
        self.write_list([[state_dict.get(name, 0) for name in group] for group in self.state_names])
    
    def write_list(self, states):
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


def main():
    ParallelSink().main()







