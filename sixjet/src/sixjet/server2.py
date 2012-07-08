
from autobus2 import Bus, wait_for_interrupt
from autobus2.net import InputThread, OutputThread
from socket import socket as Socket
from threading import Thread
from Queue import Queue
import traceback
from afn.utils import print_exceptions
from afn.utils.partial import Partial

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
        self.queue = Queue()
        self.shut_down = False
        self.socket = Socket()
        self.socket.bind(("", port))
        self.bus = Bus()
        self.service = self.bus.create_service(
                {"type": "sixjet", "sixjet.native_port": port},
                from_py_object=AutobusService(self))
        Thread(target=self.listen_for_connections).start()
    
    def listen_for_connections(self):
        # We can just infinitely loop here as run() closes the socket after
        # the event loop quits, which will cause an exception to be thrown here
        while True:
            s = self.socket.accept()
            # The connection is just a dictionary for now. We're creating it
            # before so that we can partial it into the input thread.
            connection = {"queue": Queue()}
            input_thread = InputThread(s, Partial(
                    self.remote_message_received), connection)
            output_thread = OutputThread(socket, read_function, finished_function, shut_on_end)
            connection.update({"in": input_thread}, socket=s, out=output_thread)
            self.post_event({"event": "connected"})
    
    def remote_message_received(self, message):
        self.post_event({"event": "message", "message": message})
    
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
    server = SixjetServer(None, 57952, {})
    server.start()
    wait_for_interrupt()
    print "Shutting down server..."
    server.stop()
    server.join()
    print "Server has shut down."












































