
from parallel import Parallel
from time import sleep

wait = lambda: sleep(0.001)

def write_states(states, port):
    """
    Writes the specified states, which should be a list of either booleans or
    numbers, to the specified parallel port, which should be an instance of
    parallel.Parallel (install python-parallel if you're on Debian or Ubuntu
    to get the parallel module).
    """
    # Pad the list out to 24 jets
    states = states + [0]*(24-len(states))
    # Divide the list into one list per shift register
    s1, s2, s3 = states[0:8], states[8:16], states[16:24]
    # Clear the parallel port output
    port.setData(0)
    wait()
    # Write the lists
    for i in range(8):
        # Compute the value corresponding to the data to write
        value = int(s1[i]) | (int(s2[i]) << 1) | (int(s3[i]) << 2)
        # Write the value
        port.setData(value)
        wait()
        # Set the clock high
        port.setData(value | 0b1000)
        wait()
        # Set the clock low
        port.setData(value)
        wait()
    # Set the strobe pin high
    port.setData(0b00010000)
    wait()
    # Set the strobe low
    port.setData(0)
    wait()






