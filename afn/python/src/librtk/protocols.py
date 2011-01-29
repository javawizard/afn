
from threading import Thread
import socket
import librtk
from Queue import Queue, Empty
import libautobus
import asyncore, asynchat
from traceback import print_exc
try:
    import json
except ImportError:
    import simplejson as json

"""
This module contains some protocols included with librtk.
"""

class ThreadedServer(Thread):
    def __init__(self, host, port, connect_function, **kwargs):
        """
        Creates a new ThreadedServer that will listen on the specified host
        and port and call the specified function when a new connection has
        been received. The argument to the function will be the connection
        itself.
        
        The specified keyword arguments will be passed into
        Connection.__init__. protocol and connect_function will already be
        specified, but values for any of the other arguments can be passed in.
        """
        Thread.__init__(self)
        self.host = host
        self.port = port
        self.connect_function = connect_function
        self.kwargs = kwargs
        self.socket = socket.socket()
        self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.socket.bind((host, port))
        self.socket.listen(5)
        self.running = True
    
    def run(self):
        while self.running:
            socket, _ = self.socket.accept()
            protocol = ThreadedProtocol(socket)
            connection = librtk.Connection(protocol, self.connect_function,
                    **self.kwargs)
            connection.start()
    
    def shutdown(self):
        self.running = False
        try:
            self.socket.close()
        except:
            pass

class ThreadedProtocol(object):
    def __init__(self, socket):
        self.socket = socket
    
    def protocol_init(self, connection):
        self.connection = connection
        self.out_queue = Queue()
        self.input_thread = libautobus.InputThread(self.socket,
                connection.protocol_receive, connection.protocol_connection_lost)
        self.output_thread = libautobus.OutputThread(self.socket,
                self.out_queue.get, shut_on_end=True)
        self.event_thread = librtk.EventThread()
    
    def protocol_start(self):
        self.input_thread.start()
        self.output_thread.start()
        self.event_thread.start()
    
    def protocol_send(self, data):
        self.out_queue.put(data)
    
    def protocol_event_ready(self, function):
        self.event_thread.schedule(function)
    
    def protocol_close(self):
        self.out_queue.put(None)
        self.event_thread.schedule(None)

# Async Implementation.

class AsyncDispatcher(asyncore.dispatcher):
    connection = None
    
    def __init__(self, host, port, connect_function, **kwargs):
        asyncore.dispatcher.__init__(self)
        self.create_socket(socket.AF_INET, socket.SOCK_STREAM)
        
        assert host.__class__ == str
        assert port > 0
        assert port < 65536
        
        self.host = host
        self.port = port
        self.connect_function = connect_function
        self.kwargs = kwargs
        
        self.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.bind((host, port))
        
        self.listen(5)
    
    def handle_error(self):
    	raise
    
    def handle_accept(self):
        both = self.accept()
        if both is not None:
            sock, _ = both
            protocol = AsyncProtocol(sock)
            connection = librtk.Connection(protocol, self.connect_function)
            connection.start()
    
    def poll(self, timeout=0.0, map=None):
        if map is None:
            map = asyncore.socket_map
        
        assert map.__class__ is dict
        assert timeout.__class__ in (int, float)
        
        if hasattr(asyncore.select, "poll"):
            mypoll = asyncore.poll2
        else:
            mypoll = asyncore.poll
        
        mypoll(timeout, map)
        
        # Parse the event queue.
        
        for fds, obj in map.items():
            if obj.connection is None:
                continue
            try:
                while True: # We'll raise Empty when we have nothing else to run.
                    item = obj.async_eventq.get_nowait()
                    try:
                        item()
                    except:
                        print_exc()
            except Empty, AttributeError:
                continue
    
    def loop(self, timeout=10.0, use_poll=False, map=None):
        if map is None:
            map = asyncore.socket_map
        
        while True:
            if len(map) == 0:
                break
            self.poll(timeout=timeout, map=map)

class AsyncProtocol(asynchat.async_chat):
    def __init__(self, sock):
        assert sock.__class__ is socket.socket
        
        asynchat.async_chat.__init__(self, sock=sock)
        self.set_terminator("\n")
        self.recvq = ""
    
    def handle_error(self):
    	raise
    
    def collect_incoming_data(self, data):
        self.recvq += data
    
    def get_data(self):
        data, self.recvq = self.recvq, ""
        return data
    
    def found_terminator(self):
        data = self.get_data()
        self.connection.protocol_receive(json.loads(data))

    def protocol_init(self, connection):
    	self.connection = connection
        self.async_eventq = Queue()
    
    def protocol_start(self):
        pass
    
    def protocol_send(self, data):
        self.push(json.dumps(data))
    
    def protocol_event_ready(self, function):
        self.async_eventq.put_nowait(function)
    
    def protocol_close(self):
        self.close_when_done()

class LinkedProtocol(object):
    def __init__(self, link=None):
        """
        Creates one end of a linked connection. The first of the two ends
        should be created with link omitted, and the second one should be
        created with the the first one passed in as link. In other words, you
        could construct a pair of linked protocols like this:
        
        first = LinkedProtocol()
        second = LinkedProtocol(first)
        """
        self.other = None
        self.queue = Queue() # Holds messages going from other to self
        if link is not None:
            if link.other is not None:
                raise Exception("The specified linked protocol is already "
                        "linked to another protocol object.")
            self.other = link
            link.other = self
    
    def stream(self):
        for value in iter(self.queue.get, None):
            self.connection.protocol_receive(value)
        self.connection.protocol_connection_lost()
    
    def protocol_init(self, connection):
        self.connection = connection
        self.thread = Thread(target=self.stream)
        self.event_thread = librtk.EventThread()
    
    def protocol_start(self):
        self.thread.start()
        self.event_thread.start()
    
    def protocol_send(self, data):
        self.other.queue.put(data)
    
    def protocol_event_ready(self, function):
        self.event_thread.schedule(function)
    
    def protocol_close(self):
        self.event_thread.schedule(None)
        self.queue.put(None)
        self.other.queue.put(None)















