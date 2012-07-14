"""
Autobus is a decentralized RPC system and service discovery system. You might
think of it as a mix of Bonjour and D-Bus, with a bit of JSON-RPC thrown in for
good measure.

With Autobus, various programs on a single computer or spread out over a
network can create, publish, and find each other's services. Services are
simply collections of functions that can be called, events that can be listened
for, and objects whose values can be watched. (Other types of things in
addition to those three are planned for the future.)

You're probably still a bit confused by what Autobus exactly does, so let's get
into some examples. Let's start off with the venerable Hello, World example:

# server.py
from autobus2 import Bus, wait_for_interrupt
class HelloService(object):
    def hi(self, text="world"):
        print "Saying hi to " + str(text)
        return "Hi, " + str(text) + "! How are you?"
with Bus() as bus:
    bus.create_service({"autobus.example": "hello_server"}, from_py_object=HelloService())
    wait_for_interrupt()

# client.py
from autobus2 import Bus
with Bus() as bus:
    with bus.get_service_proxy({"autobus.example": "hello_server"}) as service:
        service.wait_for_bind(timeout=2)
        print service["hi"]("great big round world")

Run server.py first, then run client.py. You'll see the server print out
"Saying hi to world" and the client print "Hello, world! How are you?". Try
running client.py first; it'll sit there waiting until you start up server.py,
at which point it will happily print out its greetings. Now try running
server.py on a completely different machine on the same local network; both
machines (one running server.py and the other running client.py) happily print
out their salutations, even though you didn't instruct, for example, client.py
which host on your network was running server.py.

So how did the client find out about the server? That's the beauty of Autobus;
it comes with service discovery built in. Notice in the client, when we call
bus.get_service_proxy (which, by the way, asks Autobus to get us an object that
knows how to find other services on the network; we'll call these service-finding
things proxies from now on), we pass it a dictionary. Notice also that when we
call bus.create_service in the server, we pass it the same dictionary.

That's how the client's finding the server; it searches the network for another
Autobus service whose dictionary (we call this the info dictionary, or the
info object, by the way) contains the keys specified in the call to get_proxy
with the values specified. (Any service containing /at least/ those keys will
match, so, for example, passing {} to bus.get_proxy would cause it to choose
any old Autobus service on the network at random)

TODO: finish this up, and go into more detail about other things like events
and objects and multiple service proxies and individual connections and
discoverers and publishers and such.  
"""

import sys
from traceback import print_exc
from autobus2 import net, discovery, local, remote, exceptions, messaging, common, proxy, service as servicemodule
from autobus2.filter import filter_matches, ANY, NOT_PRESENT
from threading import Thread, RLock
from socket import socket as Socket, error as SocketError, timeout as SocketTimeout, gethostname
from Queue import Queue
from concurrent import synchronized
import time
from utils import no_exceptions, print_exceptions
from afn.utils import singleton
from afn.utils.listener import Event, EventTable, PropertyTable
import __builtin__
from concurrent import synchronized_on
from afn.utils import field
import os
try:
    from collections import OrderedDict as _OrderedDict
except ImportError:
    from afn.backports.ordered_dict import OrderedDict as _OrderedDict
from autobus2.constants import (SYNC, THREAD, ASYNC,
        DISCOVERED, CHANGED, UNDISCOVERED)




class DiscoveredService(object):
    @field
    def locations(self):
        """
        An ordered dictionary containing the locations at which this service
        is available. The keys are (host, port) tuples representing the
        locations and the values are lists of the Discoverer instances that
        discovered those particular locations.
        """
    
    @field
    def info(self):
        """
        The info object for this service.
        """
    
    def __init__(self, info):
        self.locations = _OrderedDict()
        self.info = info


class Bus(common.AutoClose):
    """
    An Autobus bus. Busses manage a set of published services, and allow
    connecting to other services. A single bus listens on a single TCP
    port and multiplexes all published services over it.
    
    Bus is a subclass of ServiceProvider; the service it provides is a service
    exposing information about what other services, events, functions, and
    objects are present. (This service is more commonly known as the
    introspection service.) You normally won't have to know this; instances of
    Bus register themselves as services with themselves, so you don't need to
    do anything to make the introspection service work.
    """
    def __init__(self, default_discoverers=True, default_publishers=True,
                 port=None):
        """
        Creates a new bus. The bus will listen on the specified port; if none
        is specified (which is the usual case), a port will be chosen from the
        ports not currently in use on this computer.
        
        If default_discoverers is True (the default), a default set of
        discoverers will be installed, and likewise for default_publishers.
        Right now, this simply installs a autobus2.discovery.BroadcastPublisher
        and autobus2.discovery.BroadcastDiscoverer. Others might be added in
        the future.
        """
        # Number of times this bus has been __enter__'d. Allows it to be used
        # as a re-entrant context manager.
        self.context_enters = 0
        if port is None:
            port = 0
        # True once close() has been called
        self.closed = False
        # The TCP server that will listen for connections
        self.server = Socket()
        self.server.bind(("", port))
        # TODO: make the backlog configurable
        self.server.listen(100)
        self.port = self.server.getsockname()[1]
        # Lock that nearly everything bus-related locks on
        self.lock = RLock()
        # PropertyTable whose keys are service ids and whose values are
        # instances of autobus2.local.LocalService
        self.local_services = PropertyTable()
        self.local_services.global_watch(self.local_service_changed)
        # Map of ids of discovered services to DiscoveredService instances
        self.discovered_services = {}
        self.discovery_listeners = []
        # List of (filter, function) tuples, where filter is an info object
        # filter and function is a function to be notified when a matching
        # service is created or deleted
        self.service_listeners = []
        # Set of RemoteConnection instances that have been bound to a service
        self.bound_connections = set()
        # Set of discoverers registered on this bus
        self.discoverers = set()
        # Set of publishers registered on this bus
        self.publishers = set()
        if default_discoverers:
            self.install_discoverer(discovery.BroadcastDiscoverer())
        if default_publishers:
            self.install_publisher(discovery.BroadcastPublisher())
        Thread(name="autobus2.Bus.accept_loop", target=self.accept_loop).start()
        # Disable the introspection service for now. I'm seeing what would
        # happen if I have per-service introspection functions and objects, so
        # I'm disabling the bus-wide introspection service.
        # self._create_introspection_service()
        #
        # Register the bus as a service on itself.
        self.create_service({"type": "autobus.details", "pid": os.getpid()}, IntrospectionService(self))
    
    def accept_loop(self):
        """
        Called on a new thread to accept socket connections to this bus.
        """
        self.server.settimeout(1)
        while not self.closed:
            try:
                socket = None
                socket = self.server.accept()[0]
                self.setup_inbound_socket(socket)
            except SocketTimeout: # This happens when we time out, which is
                # normal. The 1-second timeout is to fix what appears to be a
                # bug with Windows not properly throwing an exception from
                # accept when another thread closes the socket.
                pass
            except: # This happens when the server socket is closed
                if socket:
                    socket.close() # Make sure it's /really/ closed on the
                    # off chance that something else caused the exception
                if not issubclass(sys.exc_type, SocketError): # Something else
                    # happened
                    print_exc()
                # print "Bus server died"
                return
    
    @synchronized_on("lock")
    def create_service(self, info, provider):
        """
        Creates a new service on this bus. info is the info object to use for
        this service. provider is the instance of
        autobus2.service.ServiceProvider to publish; an instance of
        autobus2.providers.PyServiceProvider can be used to publish a simple
        Python object as a service. (This is how I expect most services to be
        published; writing a custom ServiceProvider subclass should rarely be
        needed.)
        
        The return value is an instance of local.LocalService. You can safely
        ignore it if you don't need it and don't plan on deleting the service
        before you close the bus itself.
        """
        # Create a new id for the service
        service_id = messaging.create_service_id()
        self.set_remote_info_builtins(service_id, info)
        # Create the actual service object
        service = local.LocalService(self, service_id, info, provider)
        # Then store the service in our services map, which will cause the
        # service to be published through the introspection service and through
        # the bus's publishers (see self.local_service_changed).
        self.local_services[service_id] = service
        return service
    
    def _close_service(self, service):
        # This is called from LocalService.close, which will take care of
        # shutting down the service's connections and such. So the only thing
        # we really need to do here is delete the service from the local_service
        # map, which will cause self.local_service_changed to unpublish the
        # service and remove it from the introspection service.
        del self.local_services[service.id]
    
    @synchronized_on("lock")
    def setup_inbound_socket(self, socket):
        # Create a connection and then add it to our list of connections
        connection = local.RemoteConnection(self, socket)
        self.bound_connections.add(connection)
    
    def connect(self, host, port, service_id, timeout=10, open_listener=None,
                close_listener=None, fail_listener=None, lock=None):
        """
        Opens a connection to the specified service on the specified host/port.
        
        The connection will be returned immediately. The actual connection to
        the server will be made as soon as possible in the future. If you need
        to block until the connection actually connects, call wait_for_connect
        on the returned Connection object.
        
        The connection will attempt to reconnect indefinitely whenever it is
        disconnected. If you don't want this behavior, specify a close_listener
        that calls the connection's close method.
        
        Timeout is the TCP timeout to use when connecting. The default is 10;
        this is usually a suitable default. You'll probably only want to
        increase this if you're working on a particularly latent network.
        
        open_listener and close_listener are functions accepting one argument.
        They will be called when the connection successfully connects and when
        the connection disconnects, respectively, and the connection itself
        will be passed in. They are both run synchronously on the connection's
        input thread, so it's guaranteed that, for example, the connection will
        not attempt to reconnect until close_listener has returned. Thus
        close_listener could be set to a function that just closes the
        specified connection in order to effectively disable the auto-reconnect
        feature of connections.
        """
        return remote.Connection(self, host, port, service_id, timeout, open_listener, close_listener, fail_listener, lock)
    
    def connect_to(self, info_filter, timeout=10, open_listener=None, close_listener=None, fail_listener=None, lock=None):
        """
        Locates the first service in the list of discovered services and uses
        self.connect to connect to it. The connection is then returned.
        
        This function will be going away soon. Service proxies (which can be
        obtained using self.get_service_proxy) are the replacement; a single
        service proxy is quite similar to this method, but it can follow the
        service across restarts of the underlying process publishing the
        service, which this method can't.
        """
        with self.lock:
            for service_id, d in self.discovered_services.items():
                if filter_matches(d.info, info_filter):
                    host, port = d.locations.keys()[0]
                    return self.connect(host, port, service_id, timeout, open_listener, close_listener, fail_listener, lock)
            raise exceptions.NoMatchingServiceException()
    
    def get_service_proxy(self, info_filter, bind_function=None, unbind_function=None, multiple=False):
        """
        Returns a service proxy that will connect to services matching the
        specified info object filter. If multiple is False (the default), a
        single service proxy will be returned. If multiple is True, a multiple
        service proxy will be returned. See proxy.SingleServiceProxy and
        proxy.MultipleServiceProxy for the differences between the two.
        
        bind_function and unbind_function are optional functions that will be
        called when the proxy binds to and unbinds from a service,
        respectively. Binding is where a proxy discovers a new service matching
        its info filter and establishes a connection to it. Unbinding is where
        the proxy disconnects from said connection, usually because the service
        went away.
        """
        with self.lock:
            if multiple:
                return proxy.MultipleServiceProxy(self, info_filter, bind_function, unbind_function)
            else:
                return proxy.SingleServiceProxy(self, info_filter)
    
    @synchronized_on("lock")
    def close(self):
        """
        Closes this bus and all services registered on it.
        """
        if self.closed: # Already closed
            return
        self.closed = True
        # First we shut down all of our discoverers
        for discoverer in self.discoverers:
            discoverer.shutdown()
        # Then we need to close all of our services. Closing a service causes
        # self._close_service to be called, which removes the service from the
        # list of services, which causes self.local_service_changed to be
        # called, which unpublishes the service. So we don't need to worry
        # about unpublishing services aside from this.
        for service_id in list(self.local_services):
            self.local_services[service_id].close()
        # Then we shut down all of the publishers
        for publisher in self.publishers:
            publisher.shutdown()
        # Then we shut down the server socket
        net.shutdown(self.server)
        # Then we close all of the connections currently connected to us
        for c in self.bound_connections:
            with no_exceptions:
                c.close()
        # And that's it!
    
    @synchronized_on("lock")
    def install_publisher(self, publisher):
        # Add the publisher to our list and start it up
        self.publishers.add(publisher)
        publisher.startup(self)
        # Then register all of our local services with the publisher
        for service in self.local_services.values():
            publisher.add(service)
    
    @synchronized_on("lock")
    def remove_publisher(self, publisher):
        # Check to make sure that the publisher is already installed
        if publisher not in self.publishers:
            # TODO: Not sure why we're using __builtin__ here...
            raise __builtin__.ValueError("The specified publisher is not currently installed on this bus.")
        # Remove the publisher from our list of publishers
        self.publishers.remove(publisher)
        # Unpublish all of our services from the publisher
        for service in self.local_services.values():
            if service.active:
                publisher.remove(service)
        # Then we shut down the publisher
        publisher.shutdown()
    
    @synchronized_on("lock")
    def install_discoverer(self, discoverer):
        # Add the discoverer to our list of discoverers, then start it up
        self.discoverers.add(discoverer)
        discoverer.startup(self)
    
    @synchronized_on("lock")
    def remove_discoverer(self, discoverer):
        # Check to make sure that the discoverer has already been installed
        if discoverer not in self.discoverers:
            # TODO: Ditto from remove_publisher
            raise __builtin__.ValueError("The specified discoverer is not currently installed on this bus.")
        # Remove the discoverer from our list of discoverers, then shut it
        # down
        self.discoverers.remove(discoverer)
        discoverer.shutdown()
    
    def set_local_info_builtins(self, host, port, service_id, info):
        new_info = info.copy()
        new_info["host"] = host
        new_info["port"] = port
        new_info["service"] = service_id
        return new_info
    
    def set_remote_info_builtins(self, service_id, info):
        """
        Adds some values to the specified info object. The only one added right
        now is hostname, which is the value of socket.gethostname(). I haven't
        really standardized the list of values added here; I hope to at some
        point, though, and have all Autobus client libraries add the same ones.
        """
        info["hostname"] = gethostname()
    
    @synchronized_on("lock")
    def discover(self, discoverer, host, port, service_id, info):
        # print "Discovered:", (host, port, service_id, info)
        # Add the relevant local builtins
        info = self.set_local_info_builtins(host, port, service_id, info)
        # Check to see if the specified service has been discovered yet, and if
        # it hasn't, create an entry for it
        is_new_service = False
        if service_id not in self.discovered_services:
            self.discovered_services[service_id] = DiscoveredService(info)
            is_new_service = True
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
        # The check to see if we need to notify listeners, and do so if we
        # need to
        if is_new_service:
            self.notify_service_listeners(service_id, host, port, info, DISCOVERED) 
    
    @synchronized_on("lock")
    def undiscover(self, discoverer, host, port, service_id):
        # print "Undiscovered:", (host, port, service_id)
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
            if discovered_service.locations.keys()[0] == (host, port): # We're
                # removing the first (and therefore default) location, so if
                # there's another location, we need to let the service
                # listeners know that there's a new default location
                if len(discovered_service.locations) > 1: # There will be
                    # another location even after we delete this one
                    new_host, new_port = discovered_service.locations.keys()[1]
                    if not self.closed: # Don't issue changes if we're shutting down
                        self.notify_service_listeners(service_id, new_host, new_port, discovered_service.info, CHANGED)
            del discovered_service.locations[(host, port)]
            if not discovered_service.locations: # That was the last location
                # available for this service, so we delete the service itself,
                # and notify listeners that it was deleted
                del self.discovered_services[service_id]
                self.notify_service_listeners(service_id, host, port, discovered_service.info, UNDISCOVERED)
    
    @synchronized_on("lock")
    def add_service_listener(self, listener, info_filter=None, initial=False):
        """
        Listens for changes in services that are available. listener is a
        function listener(service_id, host, port, info, event) which will be
        called whenever a service becomes available, a service disappears, or
        the host/port that should be used to access a particular service
        changes. service_id is the id of the service; host/port is the host/port
        at which the service can be found, info is the service's info object,
        and event is one of DISCOVERED, UNDISCOVERED, or CHANGED.
        
        If info_filter is a dictionary, only services with info objects matching
        that particular filter (as per the filter_matches function) will cause
        the listener to be called. If info_filter is None (the default), or the
        empty dictionary (since all info objects match the empty dictionary),
        the listener will be called for all services.
        
        If initial is True, the listener will be immediately (and synchronously)
        called once for each service that already exists, passing in DISCOVERED
        as the event. Otherwise, the listener will only be called once the next
        
        """
        # Add the listener to our list of listeners
        self.service_listeners.append((info_filter, listener))
        # Check to see if we're supposed to notify the listener about all
        # matching services that already exist
        if initial:
            # Scan all of the services
            for service_id, discovered_service in self.discovered_services.items():
                if filter_matches(discovered_service.info, info_filter):
                    # If this service matches, notify the listener about it
                    host, port = discovered_service.locations.keys()[0]
                    with print_exceptions:
                        listener(service_id, host, port, discovered_service.info, DISCOVERED)
    
    @synchronized_on("lock")
    def remove_service_listener(self, listener, initial=False):
        # Scan the list of listeners and remove this one. Inefficient, it's
        # true, and I hope to make it more efficient later on.
        for index, (info_filter, l) in enumerate(self.service_listeners[:]):
            # See if we've hit the right listener
            if l == listener:
                # If we have, remove the listener
                del self.service_listeners[index]
                if initial:
                    # Scan through the list of services
                    for service_id, discovered_service in self.discovered_services.items():
                        if filter_matches(discovered_service.info, info_filter):
                            # This service matched, so we notify this
                            # listener that the service was removed
                            with print_exceptions:
                                listener(service_id, None, None, None, UNDISCOVERED)
                # We've found our listener and deleted it, so we return now
                return
    
    def notify_service_listeners(self, service_id, host, port, info, event):
        for filter, listener in self.service_listeners:
            if filter_matches(info, filter):
                with print_exceptions:
                    listener(service_id, host, port, info, event)
    

class IntrospectionService(servicemodule.ServiceProvider):
    def __init__(self, bus):
        self.bus = bus
        self.autobus_event = Event()
        self.bus.local_services.global_watch(self)
    
    def 


def wait_for_interrupt():
    try:
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        # print "Interrupted"
        pass
        





























