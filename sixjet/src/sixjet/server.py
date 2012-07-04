
from autobus2 import Bus, wait_for_interrupt
from parallel import Parallel
from time import sleep

# TODO: The number of jets and the number of lines and such are hard-coded
# throughout this module. Ideally they should be made into adjustable
# constants.
jet_states = [False] * 16

# The data pin for relay bank A
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


def set_parallel_data(data):
    """
    Sets the parallel port's data pins to the specified state, which should be
    a number from 0 to 255, then waits 200 microseconds.
    """
    pp.setData(data)
    time.sleep(0.000200) # 200 microseconds; increase if needed


def write_jets():
    """
    Writes the jet states stored in jet_states to the parallel port.
    """
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
    set_parallel_data(0)
    # Iterate over all the jets in reverse order. We reverse the ordering here
    # since the first bit written out will end up shifted to the last position
    # in the shift register, so we want to write the last bit first.
    for a, b in reversed(zip(jet_states[0:8], jet_states[8:16])):
        # Set lines A and B to the jets we're writing
        values = (DATA_A if a else 0) | (DATA_B if b else 0)
        set_parallel_data(values)
        # Set clock high
        set_parallel_data(values | CLOCK)
        # Set clock low
        set_parallel_data(values)
    # Set strobe high
    set_parallel_data(STROBE)
    # Set strobe low
    set_parallel_data(0)


class Service(object):
    pass


def main():
    global pp
    print "Welcome to Sixjet Server."
    print "http://hg.opengroove.org/sixjet"
    print
    print "Connecting to the parallel port..."
    pp = Parallel()
    print "Resetting jets..."
    write_jets()
    print "Starting the sixjet autobus 2 service..."
    with Bus() as bus:
        bus.create_service({"type": "sixjet"}, from_py_object=Service())
        print "Sixjet has started up. Use ^C to stop it."
        wait_for_interrupt()
        print "Shutting down the sixjet autobus 2 service..."
    print "Sixjet Server has shut down."


if __name__ == "__main__":
    main()
