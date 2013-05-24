
from stm import atomically, retry, or_else
from stm.tdict import TDict
from stm.tlist import TList
from stm.tobject import TObject
try:
    from parallel import Parallel
except:
    Parallel = None
from time import sleep
from threading import Thread
from autobus2 import Bus, wait_for_interrupt
from autobus2.providers import PyServiceProvider, PyEvent, PyObject, publish
from autobus2.net import InputThread, OutputThread
import string
import time

default_map = """
     a         b
          g

    l   m   n   h
f                   c
    k   p   o   i

          j
     e         d
"""


class StopException(Exception):
    pass


class FixedProvider(TObject):
    def __init__(self):
        TObject.__init__(self)
        self.states = TDict()
    
    def get_state(self):
        return self.states


class ComplexProvider(object):
    def __init__(self):
        self.provider_list = TList()

    def get_state(self):
        states = TDict()
        for provider in self.provider_list:
            for k, v in provider.get_state().iteritems():
                states[k] = max(states.get(k, 0), v)
        return states


class Server(TObject, ComplexProvider):
    def __init__(self):
        TObject.__init__(self)  
        ComplexProvider.__init__(self)
        self.running = True
    
    def wait_for_new_state(self, last_state, names):
        if not self.running:
            raise StopException
        server_state = self.get_state()
        new_state = TDict()
        for k in names:
            new_state[k] = server_state.get(k, 0)
        if new_state == last_state:
            retry()
        else:
            last_state.clear()
            last_state.update(new_state)
            return new_state


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
        self.last_state = atomically(TDict)
    
    def start(self):
        Thread(target=self.run).start()
    
    def run(self):
        self.write([[0 for _ in g] for g in self.state_names])
        while True:
            try:
                new_state = atomically(self.wait_for_new_state)
                self.write(new_state)
            except StopException:
                self.write([[0 for _ in g] for g in self.state_names])
                return
    
    def wait_for_new_state(self):
        # None of our fields are modified after construction, so we're ok
        # accessing them from within a transaction
        new_state = self.server.wait_for_new_state(self.last_state, self.flat_state_names)
        return [[new_state[name] for name in group] for group in self.state_names]
    
    def set_parallel_data(self, data):
        """
        Sets the parallel port's data pins to the specified state, which should be
        a number from 0 to 255, then waits a bit.
        """
        self.write_function(data)
        sleep(0.0005) # 500 microseconds; increase if needed
    
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


class ConsoleSink(object):
    def __init__(self, server, map=default_map,
                 names=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],
                 levels=".123456789^"):
        self.server = server
        self.map = map
        self.names = names
        self.levels = levels
        self.last_state = atomically(TDict)
    
    def start(self):
        Thread(target=self.run).start()

    def wait_for_new_state(self):
        # None of our fields are modified after construction, so we're ok
        # accessing them from within a transaction
        new_state = self.server.wait_for_new_state(self.last_state, self.names)
        return dict(new_state)
    
    def run(self):
        while True:
            try:
                new_state = atomically(self.wait_for_new_state)
                self.write(new_state)
            except StopException:
                self.write({})
                return
    
    def write(self, states):
        text = self.map
        for i, name in enumerate(self.names):
            text = text.replace(string.letters[i], self.levels[int((len(self.levels) - 1) * states.get(name, 0))])
        print time.ctime()
        print text


class SixjetService(ComplexProvider, PyServiceProvider):
    """
    TODO: Update the following; this class is now nothing more than an Autobus
    interface into the Server class, which does the heavy lifting.
    
    A sixjet server. Server instances listen on both a specified port for
    native sixjet commands and on an Autobus 2 service. They are created with
    a function that will be used to write bytes to the parallel port; you'll
    typically pass an instance of parallel.Parallel's setData method, but any
    function accepting an integer will do.
    """
    flash_time = PyObject("flash_time", "The time that jets will stay on when "
            "flashed with the flash method, in seconds. This can be set with "
            "set_flash_time.")
    
    def __init__(self):
        """
        Creates a new sixjet server. backend is the instance of backends.Backend
        that will be used to write jets. service_extra is an optionally-empty
        set of values that will be added to the Autobus 2's service info
        dictionary. (Keys such as type will be added automatically, but such
        keys present in service_extra will override the ones added
        automatically.)
        
        bus is the Autobus bus to use. You can usually just use:
        
        from autobus2 import Bus
        with Bus() as bus:
            server = SixjetServer(..., bus, ...)
            ...
        
        and things will work.
        """
        PyServiceProvider.__init__(self, True)
        ComplexProvider.__init__(self)
        self.flash_time = 0.25
        self.fixed_provider = atomically(FixedProvider)
        atomically(lambda: self.provider_list.append(self.fixed_provider))
    
    @publish
    def on(self, *jets):
        """
        Turns the specified jets on.
        """
        @atomically
        def _():
            for n in jets:
                self.fixed_provider.states[n] = 1
    
    @publish
    def off(self, *jets):
        """
        Turns the specified jets off.
        """
        @atomically
        def _():
            for n in jets:
                self.fixed_provider.states[n] = 0
    
    @publish
    def flash(self, *jets):
        """
        Turns the specified jets on, then turns them off after the number of
        seconds specified by the flash_time object on this service. The flash
        time can be adjusted by calling set_flash_time or update_flash_time.
        I'll probably add a mechanism later for specifying a custom flash time
        when calling flash.
        """
        flasher = atomically(lambda: Flasher(jets, self.flash_time, self.provider_list))
        flasher.schedule()
    
    @publish
    def set_flash_time(self, new_time):
        """
        Sets the flash time to the specified number of seconds, which can be a
        floating-point number.
        """
        self.flash_time = new_time
    
    @publish
    def get_flash_time(self):
        return self.flash_time
    
    @publish
    def update_flash_time(self, delta):
        """
        Adds the specified number of seconds to the current flash time. This
        can be negative to subtract from the current flash time.
        """
        self.flash_time += delta


class Flasher(object):
    def __init__(self, names, delay, provider_list):
        self.provider = FixedProvider()
        self.delay = delay
        self.provider_list = provider_list
        for n in names:
            self.provider.states[n] = 1
        provider_list.append(self.provider)
    
    def schedule(self):
        Thread(target=self.thread_main).start()
    
    def thread_main(self):
        sleep(self.delay)
        @atomically
        def _():
            self.provider_list.remove(self.provider)


def main():
    with Bus() as bus:
        server = atomically(Server)
        ConsoleSink(server).start()
        autobus_service = SixjetService()
        atomically(lambda: server.provider_list.append(autobus_service))
        bus.create_service({"type": "sixjet"}, autobus_service)
        wait_for_interrupt()
    @atomically
    def _():
        server.running = False

if __name__ == "__main__":
    main()








