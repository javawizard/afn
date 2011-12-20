
from autobus2 import net, discovery, local, remote, exceptions, messaging
from threading import Thread, RLock
from socket import socket as Socket, error as SocketError, timeout as SocketTimeout
from Queue import Queue
from concurrent import synchronized
import time
from utils import no_exceptions
from afn.utils import singleton
import __builtin__

SYNC = singleton.Singleton("autobus2.SYNC")
THREAD = singleton.Singleton("autobus2.THREAD")
ASYNC = singleton.Singleton("autobus2.ASYNC")

class Bus(object):
    def __init__(self, default_discoverers=True, default_publishers=True,
                 port=None):
        """
        Creates a new bus. The bus will listen on the specified port; if none
        is specified (which is the usual case), a port will be chosen from the
        ports not currently in use on this computer.
        """
        self.context_enters = 0
        if port is None:
            port = 0
        self.server = Socket()
        self.server.bind(("", port))
        self.server.listen(100)
        self.port = self.server.getsockname()[1]
        self.lock = RLock()
        self.local_services = {}
        self.bound_connections = set()
        self.discoverers = set()
        self.publishers = set()
        if default_discoverers:
            self.install_discoverer(discovery.BroadcastDiscoverer())
        if default_publishers:
            self.install_publisher(discovery.BroadcastPublisher())
        Thread(target=self.accept_loop).start()
    
    def accept_loop(self):
        while True:
            try:
                socket = None
                socket = self.server.accept()[0]
                self.setup_inbound_socket(socket)
            except SocketError:
                if socket:
                    socket.close()
                print "Bus server died"
                return
    
    def create_service(self, info, active=True):
        """
        Creates a new service on this bus. info is the info object to use for
        this service. active is True to publish this service immediately, False
        to wait until the returned service's activate() method is called.
        
        The return value is an instance of local.LocalService. It has methods
        such as create_function that allow functions, events, objects, and such
        to be created on the service.
        """
        with self.lock:
            service_id = messaging.create_service_id()
            service = local.LocalService(self, service_id, info)
            self.local_services[service_id] = service
            if active:
                service.activate()
            return service
    
    def setup_inbound_socket(self, socket):
        with self.lock:
            connection = local.RemoteConnection(self, socket)
            self.bound_connections.add(connection)
    
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
        except SocketError as s:
            raise exceptions.ConnectionException(s)
        return remote.Connection(self, s, service_id)
    
    def close(self):
        with self.lock:
            for publisher in self.publishers:
                for service in self.local_services.values():
                    if service.active:
                        publisher.remove(service)
                publisher.shutdown()
            for discoverer in self.discoverers:
                discoverer.shutdown()
            net.shutdown(self.server)
            for c in self.bound_connections:
                with no_exceptions:
                    c.close()
    
    def __enter__(self):
        self.context_enters += 1
        return self
    
    def __exit__(self, *args):
        self.context_enters -= 1
        if self.context_enters == 0:
            self.close()
    
    def install_publisher(self, publisher):
        with self.lock:
            self.publishers.add(publisher)
            publisher.startup(self)
            for service in self.local_services.values():
                if service.active:
                    publisher.add(service)
    
    def remove_publisher(self, publisher):
        with self.lock:
            if publisher not in self.publishers:
                raise __builtin__.ValueError("The specified publisher is not currently installed on this bus.")
            self.publishers.remove(publisher)
            for service in self.local_services.values():
                if service.active:
                    publisher.remove(service)
            publisher.shutdown()
    
    def install_discoverer(self, discoverer):
        with self.lock:
            self.discoverers.add(discoverer)
            discoverer.startup(self)
            # TODO: finish this up
    
    def remove_discoverer(self, discoverer):
        with self.lock:
            if discoverer not in self.discoverers:
                raise __builtin__.ValueError("The specified discoverer is not currently installed on this bus.")
            self.discoverers.remove(discoverer)
            discoverer.shutdown()
    
    def discover(self, discoverer, host, port, service_id, info):
        print "Discovered:", (host, port, service_id, info)
    
    def undiscover(self, discoverer, host, port, service_id):
        print "Undiscovered:", (host, port, service_id)


def wait_for_interrupt():
    try:
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        print "Interrupted"
        





























