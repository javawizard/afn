
from select import select
from Queue import Queue, Empty
from autobus2 import exceptions
from socket import socket as Socket, SHUT_RDWR, error as SocketError
from threading import Thread, RLock, Condition
from traceback import print_exc
import json
from utils import no_exceptions


class _InProgressSocketManager(object):
    """
    This class does not actually work right now. It's something that I'm
    writing that will some day replace InputThread and OutputThread and allow
    Autobus to function using only a single thread for each Bus instance. So
    it's a work in progress, and it doesn't actually work right now.
    """
    def __init__(self):
        # Set up the interrupt socket
        interrupt_server = Socket()
        interrupt_server.bind(("localhost", 0))
        interrupt_server.listen(1)
        self.interrupt_writer = Socket()
        self.interrupt_writer.setblocking(False)
        self.interrupt_writer.connect("localhost", interrupt_server.getsockname()[1])
        self.interrupt_reader = interrupt_server.accept()
        interrupt_server.shutdown(SHUT_RDWR)
        interrupt_server.close()
        self.interrupt_reader.setblocking(False)
        self.interrupt_writer.setblocking(False)
    
    def loop(self):
        pass
    
    def run_sync(self, function, timeout=None):
        q = Queue()
        @self.run_async
        def new_function():
            q.put(function())
        try:
            return q.get(timeout=timeout)
        except Empty:
            raise exceptions.TimeoutException
    
    def run_async(self, function):
        pass
    
    def interrupt(self):
        self.interrupt_writer.send(0)


def linesplit(socket):
    """
    A generator that yields lines of text from the socket.
    """
    buffer = socket.recv(512)
    done = False
    while not done:
        if "\n" in buffer:
            line, buffer = buffer.split("\n", 1)
            if line[-1:] == "\r":
                line = line[:-1]
            if line.strip(): # Ignore blank lines
                yield line
        else:
            more = socket.recv(512)
            if not more:
                done = True
            else:
                buffer = buffer + more
    if buffer[-1:] == "\r":
        buffer = buffer[:-1]
    if buffer:
        yield buffer


class InputThread(Thread):
    """
    A thread that reads messages from a socket and calls a user-defined
    function with the received messages. When the connection dies, it closes
    the socket and calls the function with None as its argument.
    
    The user-defined message function can be changed at any time by assigning
    to the function property of any given input thread. If None is assigned to
    it (or specified when constructing the input thread), the input thread will
    block after it receives a message until a non-None function is assigned to
    the function property.
    """
    def __init__(self, socket, function):
        Thread.__init__(self)
        self.socket = socket
        self.generator = linesplit(socket)
        self.lock = RLock()
        self.condition = Condition(self.lock)
        self.function = function
    
    @property
    def function(self):
        return self._function
    
    @function.setter
    def function(self, new_function):
        with self.lock:
            self._function = new_function
            self.condition.notify_all()
    
    def run(self):
        try:
            while True:
                message_data = self.generator.next()
#                print "Message received, decoding..."
                message = json.loads(message_data)
#                print "Dispatching received message..."
                f = self.get_function_or_wait()
                f(message)
        except EOFError:
            pass
        except StopIteration:
            pass
        except SocketError:
            pass
        except:
            print_exc()
#        print "Input thread finished, closing socket..."
        self.socket.close()
        f = self.get_function_or_wait()
        f(None)
    
    def get_function_or_wait(self):
        with self.lock:
            f = self.function
            while f is None:
                self.condition.wait()
                f = self.function
        return f


class OutputThread(Thread):
    """
    A thread that repeatedly calls a function which should return a message
    object to send. The function can (and generally should) block as needed.
    This thread then writes the message to the socket. When the socket dies,
    it will be closed. If the function returns None, the socket will be
    immediately closed and this thread will exit.
    
    finished_function will be called after every message has been successfully
    written out. It defaults to lambda: None.
    
    The read function can return either a json object or a string. If it's
    the former, it will be converted to a string by using the json module's
    json.dumps function. Messages are ordinarily returned as json
    objects to prevent message serialization from blocking the event queue in
    Autobus, but in some cases (such as when a single message is to be sent to
    several recipients), it may be more efficient to pre-encode the message
    and send it as a string.
    """
    def __init__(self, socket, read_function, finished_function=lambda: None,
            shut_on_end=False):
        Thread.__init__(self)
        self.socket = socket
        self.read_function = read_function
        self.finished_function = finished_function
        self.enable_buffer_hack = False
        self.shut_on_end=shut_on_end
    
    def run(self):
        try:
            while True:
#                print "Waiting for a message to send..."
                message = self.read_function()
                try:
#                print "Getting ready to send message " + str(message)
                    if message is None:
#                        print "Message was empty, shutting down the thread"
                        break
                    if isinstance(message, str):
                        message_data = message
                    else:
                        message_data = json.dumps(message)
                    self.socket.sendall(message_data + "\r\n")
                    if self.enable_buffer_hack:
                        self.socket.sendall((" "*5120) + "\r\n")
                finally:
                    self.finished_function()
        except SocketError:
            pass
        except:
            print_exc()
        if self.shut_on_end:
            try:
                self.socket.shutdown(SHUT_RDWR)
            except:
                pass
        try:
            self.socket.close()
        except:
            pass


def start_io_threads(socket, input_function, output_function):
    input_thread = InputThread(socket, input_function)
    output_thread = OutputThread(socket, output_function)
    input_thread.start()
    output_thread.start()
    return input_thread, output_thread


def shutdown(socket):
    with no_exceptions:
        socket.shutdown(SHUT_RDWR)
        socket.close()


def sendto(socket, data, address):
    """
    Sends the specified data, which should be a str, to the specified address,
    which is typically a (host, port) tuple, using the specified socket (which
    generally should be a UDP datagram socket). This basically just invokes
    socket.sendto(data, address), except that it catches and ignores any
    exceptions thrown while sending, which is useful because sending to
    255.255.255.255 when the network interface is down will cause an exception
    to be thrown.
    """
    with no_exceptions:
        socket.sendto(data, address)


def ensure_jsonable(value):
    try:
        json.dumps(value)
    except:
        raise exceptions.InvalidValueException()





























