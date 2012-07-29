
from autobus2 import Bus, wait_for_interrupt
from autobus2.providers import PyServiceProvider, PyEvent, PyObject, publish
from autobus2.net import InputThread, OutputThread
from socket import socket as Socket
from threading import Thread
from Queue import Queue
import traceback
from afn.utils import eventloop, print_exceptions
from afn.utils.partial import Partial
from afn.utils.singleton import Singleton
from afn.backports.argparse import ArgumentParser
from time import sleep
from time import time as current_time
import sys

MANUAL = Singleton("sixjet.server2.MANUAL")

# TODO: The number of jets and the number of lines and such are hard-coded
# throughout this module. Ideally they should be made into adjustable
# constants.

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


class SixjetServer(PyServiceProvider):
    """
    A sixjet server. Server instances listen on both a specified port for
    native sixjet commands and on an Autobus 2 service. They are created with
    a function that will be used to write bytes to the parallel port; you'll
    typically pass an instance of parallel.Parallel's setData method, but any
    function accepting an integer will do.
    """
    flash_time = PyObject("flash_time", "The time that jets will stay on when "
            "flashed with the flash method, in seconds. This can be set with "
            "set_flash_time.")
    
    def __init__(self, backend, bus, service_extra={}):
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
        service_extra = service_extra.copy()
        service_extra.update(type="sixjet")
        self.backend = backend
        # The event loop used by this server.
        self.loop = eventloop.EventLoop()
        # The current states of all of the jets. This must only be read and
        # modified from the event thread.
        self.states = [False] * 16
        # True to shut down the server. This shouldn't be modified; instead,
        # stop() should be called, which will post an event that sets this to
        # True.
        self.shut_down = False
        self.service = self.bus.create_service(service_extra, self)
        self.loop.start()
        # Flush the jets to turn all of them off, but do it on the event loop
        self.loop.run(Partial(self.flush))
    
    def start(self):
        self.loop.start()
    
    def stop(self):
        self.loop.shutdown()
    
    def join(self):
        self.loop.join()
    
    def flush(self):
        self.backend.write(self.states)
    
    # And now for the functions that are published via Autobus.
    
    @publish
    @eventloop.on("server.loop")
    def on(self, *jets):
        """
        Turns the specified jets on.
        """
        for n in jets:
            # Cancel all manual scheduled events for this jet
            self.loop.cancel((MANUAL, n))
            # Turn the jet on
            self.states[n] = True
        # Write the new states to the parallel port
        self.flush()
    
    @publish
    @eventloop.on("server.loop")
    def off(self, *jets):
        """
        Turns the specified jets off.
        """
        for n in jets:
            # Cancel all manual scheduled events for this jet
            self.loop.cancel((MANUAL, n))
            # Turn the jet off
            self.states[n] = False
        # Write the new states to the parallel port
        self.flush()
    
    @publish
    @eventloop.on("server.loop")
    def flash(self, *jets):
        """
        Turns the specified jets on, then turns them off after the number of
        seconds specified by the flash_time object on this service. The flash
        time can be adjusted by calling set_flash_time or update_flash_time.
        I'll probably add a mechanism later for specifying a custom flash time
        when calling flash.
        """
        # Figure out what time the jets should turn off
        off_time = current_time() + self.flash_time
        for n in jets:
            # Cancel all manual scheduled events for this jet
            self.loop.cancel((MANUAL, n))
            # Turn the jet on
            self.states[n] = True
            # Schedule an event to turn this jet off. We assign it the
            # categories MANUAL and (MANUAL, n); the former is used to cancel
            # all manual events when switching to automatic mode, and the
            # latter is used by other calls to flash (and calls to on/off) to
            # cancel other flash-related events for the jet. 
            self.loop.schedule(Partial(self.off, n), off_time,
                    MANUAL, (MANUAL, n))
        # Write the new states to the parallel port
        self.flush()
    
    @publish
    @eventloop.on("server.loop")
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
    @eventloop.on("server.loop")
    def update_flash_time(self, delta):
        """
        Adds the specified number of seconds to the current flash time. This
        can be negative to subtract from the current flash time.
        """
        self.flash_time += delta


if __name__ == "__main__":
    sys.argv
    with Bus() as bus:
        server = SixjetServer(None, bus)
        server.start()
        wait_for_interrupt()
        print "Shutting down server..."
        server.stop()
        server.join()
        print "Shutting down bus..."
    print "Server has shut down."












































