
from autobus2 import Bus, wait_for_interrupt
from autobus2.net import InputThread, OutputThread
from socket import socket as Socket
from parallel import Parallel
from threading import Thread
from Queue import Queue
import traceback
from afn.utils import print_exceptions

class AutobusService(object):
    def __init__(self, server):
        self._server = server
    
    def on(self, *jets):
        self._server.post_event({"event": "message", "message":
                {"command": "set", "on": jets}})
    
    def off(self, *jets):
        self._server.post_event({"event": "message", "message":
                {"command": "set", "off": jets}})


class SixjetServer(Thread):
    """
    A sixjet server. Server instances listen on both a specified port for
    native sixjet commands and on an Autobus 2 service. They are created with
    a function that will be used to write bytes to the parallel port; you'll
    typically pass an instance of parallel.Parallel's setData method, but any
    function accepting an integer will do.
    """
    def __init__(self, write_function, port, service_extra={}):
        """
        Creates a new sixjet server. write_function is the function to use to
        write data to the parallel port. port is the port on which the native
        protocol listener should listen. service_extra is an optionally-empty
        set of values that will be added to the Autobus 2's service info
        dictionary. (Keys such as type will be added automatically, but such
        keys present in service_extra will override the ones added
        automatically.)
        """
        Thread.__init__(self)
        self.queue = Queue()
        self.shut_down = False
        self.socket = Socket()
        self.socket.bind(("", port))
        self.bus = Bus()
        self.service = self.bus.create_service(
                {"type": "sixjet", "sixjet.native_port": port},
                from_py_object=AutobusService(self))
    
    def run(self):
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
        with print_exceptions:
            self.bus.close()
    
    def stop(self):
        self.post_event({"event": "stop"})
    
    def post_event(self, event):
        self.queue.put(event)
    
    def handle_event(self, event):
        if event["event"] == "message":
            self.handle_message(event["message"])
        elif event["event"] == "stop":
            self.shut_down = True
        else:
            print "Warning: unrecognized event type: %r" % event
    
    def handle_message(self, message):
        if message["command"] == "set":
            for n in message["on"]:
                pass
            for n in message["off"]:
                pass


if __name__ == "__main__":
    p = Parallel()
    server = SixjetServer(p.setData, 57952, {})
    server.start()
    wait_for_interrupt()
    print "Shutting down server..."
    server.stop()
    server.join()
    print "Server has shut down."












































