
from autobus2 import net, discovery, local, remote, exceptions
from threading import Thread, RLock
from socket import socket as Socket, SHUT_RDWR, error as SocketError, timeout as SocketTimeout
from Queue import Queue
from concurrent import synchronized




class Bus(object):
    def __init__(self, default_discoverers=True, default_publishers=True,
                 port=None):
        """
        Creates a new bus. The bus will listen on the specified port; if none
        is specified (which is the usual case), a port will be chosen from the
        ports not currently in use on this computer.
        """
        if port is None:
            port = 0
        self.server = Socket()
        self.server.bind(("", port))
        self.server.listen(100)
        self.port = self.server.getsockname()[1]
        self.lock = RLock()
        Thread(target=self.accept_loop).start()
    
    def accept_loop(self):
        while True:
            try:
                socket = self.server.accept()
                self.setup_inbound_socket(socket)
            except SocketError:
                socket.close()
                print "Bus server died"
                return
    
    def setup_inbound_socket(self, socket):
        # TODO: actually implement this
        socket.close()
    
    def connect(self, host, port, service_id, timeout=10):
        """
        Connects to the specified service on the specified host and port.
        
        The specified timeout will be used while establishing a TCP connection.
        If a connection could not be established in the specified amount of
        time, a TimeoutException will be thrown.
        """
        s = Socket()
        s.settimeout(timeout)
        try:
            s.connect((host, port))
        except SocketTimeout:
            raise exceptions.TimeoutException
        





























