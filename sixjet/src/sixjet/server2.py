
from autobus2 import Bus, wait_for_interrupt
from autobus2.net import InputThread, OutputThread
from socket import socket as Socket
from threading import Thread
from Queue import Queue
import traceback
from afn.utils import print_exceptions
from afn.utils.partial import Partial
from time import sleep

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


class AutobusService(object):
    def __init__(self, server):
        self._server = server
    
    def on(self, *jets):
        self._server.post_event({"event": "message", "message":
                {"command": "set", "on": jets}})
    
    def off(self, *jets):
        self._server.post_event({"event": "message", "message":
                {"command": "set", "off": jets}})


class Connection(object):
    """
    A remote native-protocol connection.
    """
    def __init__(self, server, socket):
        self.server = server
        self.socket = socket
        self.queue = Queue()
        self.input_thread = InputThread(socket, self.message_received)
        self.output_thread = OutputThread(socket, self.queue.get)
    
    def start(self):
        self.input_thread.start()
        self.output_thread.start()
    
    def message_received(self, message):
        if message is None:
            self.server.post_event({"event": "disconnected",
                    "connection": self})
        self.server.remote_message_received(self, message)
    
    def send(self, message):
        self.queue.put(message)


class SixjetServer(Thread):
    """
    A sixjet server. Server instances listen on both a specified port for
    native sixjet commands and on an Autobus 2 service. They are created with
    a function that will be used to write bytes to the parallel port; you'll
    typically pass an instance of parallel.Parallel's setData method, but any
    function accepting an integer will do.
    """
    def __init__(self, write_function, port, bus, service_extra={}):
        """
        Creates a new sixjet server. write_function is the function to use to
        write data to the parallel port. port is the port on which the native
        protocol listener should listen. service_extra is an optionally-empty
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
        
        If write_function is None, a new parallel.Parallel instance will be
        created, and its setData method used.
        """
        Thread.__init__(self)
        if write_function is None:
            import parallel
            self._parallel = parallel.Parallel()
            write_function = self._parallel.setData
        else:
            self._parallel = None
        # The event queue. Events can be pushed from any thread, but can only
        # be read and processed from the SixjetServer's run method.
        self.queue = Queue()
        # The list of remote native-protocol connections. This must only be
        # read and modified from the event thread.
        self.connections = []
        # The current states of all of the jets. This must only be read and
        # modified from the event thread.
        self.jet_states = [False] * 16
        # True to shut down the server. This shouldn't be modified; instead,
        # stop() should be called, which will post an event that sets this to
        # True.
        self.shut_down = False
        # The server socket listening on the native protocol port
        self.socket = Socket()
        self.socket.bind(("", port))
        # The Autobus service we're publishing
        self.service = self.bus.create_service(
                {"type": "sixjet", "sixjet.native_port": port},
                from_py_object=AutobusService(self))
    
    def listen_for_connections(self):
        # We can just infinitely loop here as run() closes the socket after
        # the event loop quits, which will cause an exception to be thrown here
        while True:
            s = self.socket.accept()
            # The connection is just a dictionary for now. We're creating it
            # before so that we can partial it into the input thread.
            connection = Connection(self, s)
            self.post_event({"event": "connected", "connection": connection})
            connection.start()
    
    def remote_message_received(self, connection, message):
        self.post_event({"event": "message", "message": message})
    
    def run(self):
        # Start a socket listening for connections
        Thread(target=self.listen_for_connections).start()
        # Read events and process them in a loop.
        while not self.shut_down:
            # TODO: add support for scheduled tasks here, or use separate
            # threads to post events when they happen. The latter would offer
            # a better guarantee that events will be processed when they happen
            # due to Python's sleep-waiting whenever using get with a timeout
            # (which is due to attempting to wait on a condition with a timeout
            # doing the aforementioned).
            event = self.queue.get()
            try:
                self.handle_event(event)
            except:
                traceback.print_exc()
            finally:
                self.queue.task_done()
        with print_exceptions:
            self.socket.close()
    
    def stop(self):
        self.post_event({"event": "stop"})
    
    def post_event(self, event):
        self.queue.put(event)
    
    def handle_event(self, event):
        if event["event"] == "message":
            self.handle_message(event["message"])
        elif event["event"] == "stop":
            self.shut_down = True
        elif event["event"] == "connected":
            self.connections.append(event["connection"])
        elif event["event"] == "disconnected":
            self.connections.remove(event["connection"])
        else:
            print "Warning: unrecognized event type: %r" % event
    
    def handle_message(self, message):
        if message["command"] == "set":
            for n in message["on"]:
                pass
            for n in message["off"]:
                pass

    def set_parallel_data(self, data):
        """
        Sets the parallel port's data pins to the specified state, which should be
        a number from 0 to 255, then waits a bit.
        """
        self.write_function(data)
        sleep(0.0032) # 3.2 milliseconds; increase if needed
    
    def write_jets(self):
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


if __name__ == "__main__":
    server = SixjetServer(None, 57952, {})
    server.start()
    wait_for_interrupt()
    print "Shutting down server..."
    server.stop()
    server.join()
    print "Server has shut down."












































