
from autobus2 import net, discovery, local, remote, exceptions, messaging
from threading import Thread, RLock
from socket import socket as Socket, error as SocketError, timeout as SocketTimeout
from Queue import Queue
from concurrent import synchronized
import time
from utils import no_exceptions
from afn.utils import singleton
import __builtin__
import collections

SYNC = singleton.Singleton("autobus2.SYNC")
THREAD = singleton.Singleton("autobus2.THREAD")
ASYNC = singleton.Singleton("autobus2.ASYNC")
ANY = singleton.Singleton("autobus2.ANY")
NOT_PRESENT = singleton.Singleton("autobus2.NOT_PRESENT")
DISCOVERED = singleton.Singleton("autobus2.DISCOVERED")
UNDISCOVERED = singleton.Singleton("autobus2.UNDISCOVERED")
CHANGED = singleton.Singleton("autobus2.CHANGED")

def filter_matches(info, filter):
    for k, v in filter.items():
        if v is ANY:
            if k not in info:
                return False
        elif v is NOT_PRESENT:
            if k in info:
                return False
        else:
            if info[k] != v:
                return False
    return True


class DiscoveredService(object):
    def __init__(self, info):
        self.locations = collections.OrderedDict()
        self.info = info


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
        self.discovered_services = {}
        self.discovery_listeners = []
        self.service_listeners = []
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
            except SocketError: # This happens when the server socket is closed
                if socket:
                    socket.close() # Make sure it's /really/ closed on the
                    # off chance that something else caused the exception
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
            # Create a new id for the service
            service_id = messaging.create_service_id()
            # Create the actual service object
            service = local.LocalService(self, service_id, info)
            # Then store the service in our services map
            self.local_services[service_id] = service
            # If the service is to be immediately activated it, then we should
            # do so
            if active:
                service.activate()
            return service
    
    def setup_inbound_socket(self, socket):
        with self.lock:
            # Create a connection and then add it to our list of connections
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
            # First we need to unpublish all of our services and shut down all
            # of our publishers
            for publisher in self.publishers:
                for service in self.local_services.values():
                    if service.active:
                        publisher.remove(service)
                publisher.shutdown()
            # Then we shut down all of our discoverers
            for discoverer in self.discoverers:
                discoverer.shutdown()
            # Then we shut down the server socket
            net.shutdown(self.server)
            # Then we close all of the connections currently connected to us
            for c in self.bound_connections:
                with no_exceptions:
                    c.close()
            # And that's it!
            # TODO: In the future, store some sort of closed field so that if
            # someone tries to double-close us, we can tell and just ignore it
            # the second time
    
    def __enter__(self):
        # Increment the number of context entrances
        self.context_enters += 1
        return self
    
    def __exit__(self, *args):
        # Decrement the number of context entrances
        self.context_enters -= 1
        # If the number of entrances is 0, close the bus
        if self.context_enters == 0:
            self.close()
    
    def install_publisher(self, publisher):
        with self.lock:
            # Add the publisher to our list and start it up
            self.publishers.add(publisher)
            publisher.startup(self)
            # Then register all of our local services with the publisher
            for service in self.local_services.values():
                if service.active:
                    publisher.add(service)
    
    def remove_publisher(self, publisher):
        with self.lock:
            # Check to make sure that the publisher is already installed
            if publisher not in self.publishers:
                raise __builtin__.ValueError("The specified publisher is not currently installed on this bus.")
            # Remove the publisher from our list of publishers
            self.publishers.remove(publisher)
            # Unpublish all of our services from the publisher
            for service in self.local_services.values():
                if service.active:
                    publisher.remove(service)
            # Then we shut down the publisher
            publisher.shutdown()
    
    def install_discoverer(self, discoverer):
        with self.lock:
            # Add the discoverer to our list of discoverers, then start it up
            self.discoverers.add(discoverer)
            discoverer.startup(self)
    
    def remove_discoverer(self, discoverer):
        with self.lock:
            # Check to make sure that the discoverer has already been installed
            if discoverer not in self.discoverers:
                raise __builtin__.ValueError("The specified discoverer is not currently installed on this bus.")
            # Remove the discoverer from our list of discoverers, then shut it
            # down
            self.discoverers.remove(discoverer)
            discoverer.shutdown()
    
    def discover(self, discoverer, host, port, service_id, info):
        print "Discovered:", (host, port, service_id, info)
        # Check to see if the specified service has been discovered yet, and if
        # it hasn't, create an entry for it
        if service_id not in self.discovered_services:
            self.discovered_services[service_id] = DiscoveredService(info)
        discovered_service = self.discovered_services[service_id]
        # Check to see if the specified host/port combination is already
        # present, and if it isn't, add it.
        if (host, port) not in discovered_service.locations:
            discovered_service.locations[(host, port)] = []
        discoverer_list = discovered_service.locations[(host, port)]
        # Check to see if this discoverer has already discovered that host/port
        if discoverer in discoverer_list:
            print ("Warning: discoverer " + str(discoverer) + 
                   " tried to rediscover " + str((host, port, service_id)) +
                   " with info " + str(info))
            return
        # It hasn't, so add it.
        discoverer_list.append(discoverer) 
    
    def undiscover(self, discoverer, host, port, service_id):
        print "Undiscovered:", (host, port, service_id)
        # Check to see if the specified service has been discovered.
        if service_id not in self.discovered_services:
            print ("Warning: discoverer " + str(discoverer) + " tried to "
                   "undiscover " + str((host, port, service_id)) + " when "
                   "such a service does not exist.")
            return
        discovered_service = self.discovered_services[service_id]
        if (host, port) not in discovered_service.locations:
            print ("Warning: discoverer " + str(discoverer) + " tried to "
                   "undiscover " + str((host, port, service_id)) + " when "
                   "that host/port has not yet been discovered.")
            return
        discoverer_list = discovered_service.locations[(host, port)]
        if discoverer not in discoverer_list:
            print ("Warning: discoverer " + str(discoverer) + " tried to "
                   "undiscover " + str((host, port, service_id)) + " when "
                   "this discoverer hasn't discovered that host/port yet.")
            return
        discoverer_list.remove(discoverer)
        if not discoverer_list:
            del discovered_service.locations[(host, port)]
            if not discovered_service.locations:
                del self.discovered_services[service_id]
    
    def add_discovery_listener(self, listener, info_filter=None, initial=False):
        self.discovery_listeners.append((info_filter, listener))
    
    def remove_discovery_listener(self, listener, initial=False):
        for index, (info_filter, l) in enumerate(self.discovery_listeners[:]):
            if l == listener:
                del self.discovery_listeners[index]
                return
    
    def add_service_listener(self, listener, info_filter=None, initial=False):
        self.service_listeners.append((info_filter, listener))
    
    def remove_service_listener(self, listener):
        for index, (info_filter, l) in enumerate(self.service_listeners[:]):
            if l == listener:
                del self.service_listeners[index]
                return


def wait_for_interrupt():
    try:
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        print "Interrupted"
        





























