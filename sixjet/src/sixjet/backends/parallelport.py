
from parallel import Parallel
from time import sleep

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


class ParallelBackend(object):
    def __init__(self, write_function):
        self.write_function = write_function
    
    def set_parallel_data(self, data):
        """
        Sets the parallel port's data pins to the specified state, which should be
        a number from 0 to 255, then waits a bit.
        """
        # self.loop.ensure_event_thread()
        self.write_function(data)
        sleep(0.0026) # 2.6 milliseconds; increase if needed
    
    def write(self, states):
        self.write_actual(states)
        self.write_actual(states)
    
    def write_actual(self, states):
        """ 
        Writes the jet states stored in jet_states to the parallel port.
        """
        self.jet_states = states
        # self.loop.ensure_event_thread()
        # The sixjet board is basically made up of two 74HC595 8-bit shift
        # registers. For those not familiar with shift registers, they're basically
        # a queue of bits; new bits can be pushed in at one end, and when the queue
        # is full, old bits will be dropped from the other end. They can then be
        # instructed to take the bits currently in the queue and set 8 of their
        # pins to those values. They're perfect for controlling things like banks
        # of relays from the parallel port.
        # To push a bit into the queue, you set the shift register's DATA pin to
        # 0 if you want to push a 0 and 1 if you want to push a 1. Then you set the
        # CLOCK pin high and then back low.
        # To have the shift register take the bits in the queue and set its eight
        # output pins to their values, you set the shift register's STROBE pin high
        # and then low.
        # On the 74HC595 shift register, pin 11 is CLOCK, pin 12 is STORBE, and pin
        # 15 is DATA. Googling "74HC595 datasheet" will pull up a map of which pin
        # numbers are which physical pins on the 74HC595.
        # The sixjet board has two shift registers that each control a bank of 8
        # relays. The module constants DATA_A and DATA_B correspond to the data
        # pins of each of these shift registers. CLOCK and STROBE are connected to
        # both shift registers' clock and strobe pins.
        # So, to write out the data... 
        # Clear the parallel port
        self.set_parallel_data(0)
        # Iterate over all the jets in reverse order. We reverse the ordering here
        # since the first bit written out will end up shifted to the last position
        # in the shift register, so we want to write the last bit first.
        for a, b in reversed(zip(self.jet_states[0:8], self.jet_states[8:16])):
            # Set lines A and B to the jets we're writing
            values = (DATA_A if a else 0) | (DATA_B if b else 0)
            self.set_parallel_data(values)
            # Do it an extra time just to see if it helps some issues I've been
            # seeing with data occasionally getting clocked in wrong
            self.set_parallel_data(values)
            # Set clock high
            self.set_parallel_data(values | CLOCK)
            # Set clock low
            self.set_parallel_data(values)
        # Set strobe high
        self.set_parallel_data(STROBE)
        # Set strobe low
        self.set_parallel_data(0)



