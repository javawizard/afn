"""
This module primarily contains Autobus's service discovery code.
"""

from abc import ABCMeta as ABC, abstractmethod as abstract
from socket import socket as Socket, AF_INET, SOCK_DGRAM, SOL_SOCKET, SO_REUSEADDR, SO_BROADCAST, timeout as SocketTimeout, error as SocketError
from autobus2 import constants, net, exceptions
from threading import Thread
import json
from traceback import print_exc
import time
import random
from Queue import Queue


class Discoverer(object):
    """
    A class capable of discovering services available remotely and making a
    particular bus aware of those services.
    
    This class is abstract; one of its subclases must be used instead. Bus
    instances, by default, install a BroadcastDiscoverer when they are created.
    
    When a discoverer is started, it should start attempting to discover
    services, however it goes about doing that. When it discovers one, it
    should call bus.discovered(self, host, port, service_id, info_object).
    When one disappears, it should call bus.undiscovered, passing in the same
    parameters. If the info object changes, the discoverer should just call
    bus.discovered again with the new info object.
    """
    @abstract
    def startup(self, bus):
        """
        """
    
    @abstract
    def shutdown(self):
        """
        """


class BroadcastDiscoverer(object):
    """
    An implementation of Discoverer that listens for UDP broadcasts sent by
    other BroadcastPublisher instances on the network.
    """
    def __init__(self):
        self.running = True
        # Maps (host, port, service_id) to [info_object, last_alive_time]
        self.services = {}
        self.last_check_time = 0
    
    def startup(self, bus):
        if not self.running:
            raise Exception("BroadcastDiscoverers can only be used once")
        self.bus = bus
        self.sender, self.receiver = create_broadcast_sockets()
        Thread(name="autobus2.discovery.BroadcastDiscoverer.receive_loop", 
                target=self.receive_loop).start()
        Thread(name="autobus2.discovery.BroadcastDiscoverer.send_initial_requests", 
                target=self.send_initial_requests).start()
    
    def shutdown(self):
        with self.bus.lock:
            for k in self.services:
                self.bus.undiscover(self, *k)
            self.services.clear()
        self.running = False
        net.shutdown(self.receiver)
    
    def receive_loop(self):
        while True:
            message = None
            try:
                message, (host, port) = self.receiver.recvfrom(16384)
            except SocketError:
                pass
            if not self.running:
                net.shutdown(self.sender)
                return
            self.run_recurring()
            if not message:
                continue
            try:
                message = json.loads(message)
                if message["command"] == "add":
                    self.process_add(host, message["port"], message["service"], message["info"])
                elif message["command"] == "remove":
                    self.process_remove(host, message["port"], message["service"])
            except:
                print "Couldn't read message"
                print_exc()
                continue
    
    def send_initial_requests(self):
        for delay in constants.query_initial_intervals:
            for i in range(int(delay*10.0)): # Sleep in 100ms blocks so that if the
                # discoverer is shut down in the middle, we'll terminate fairly
                # quickly
                time.sleep(0.1)
                if not self.running:
                    return
            net.sendto(self.sender, json.dumps({"command": "query"}),
                    ("127.255.255.255", constants.broadcast_port))
            net.sendto(self.sender, json.dumps({"command": "query"}),
                    ("255.255.255.255", constants.broadcast_port))
    
    def run_recurring(self):
        check_interval = 12 # TODO: change this to more like 30, but make sure
        # it's greater than the Autobus connect timeout so that we don't get
        # two separate attempts to connect to the same service going on at the
        # same time
        expire_interval = 30 # TD: change this to more like 60
        current_time = time.time()
        if self.last_check_time + check_interval > current_time:
            return
        self.last_check_time = current_time
        # print "Checking discovered services..."
        with self.bus.lock:
            for k, v in self.services.iteritems():
                if v[1] + expire_interval < current_time: # Haven't received a
                    # message from this server in a while, so check to see if
                    # we can connect to it
                    Thread(name="autobus2.discovery.BroadcastDiscoverer.try_to_connect",
                            target=self.try_to_connect, args=(k, v)).start()
    
    def try_to_connect(self, k, v):
        host, port, service_id = k
        # print "Discovery timeout, attempting to connect to " + str(k)
        failed = False
        try:
            with self.bus.connect(*k) as connection:
                connection.wait_for_connect(10)
        except (exceptions.ConnectionException, exceptions.TimeoutException) as e:
            # print "Service could not be connected to, removing..."
            failed = True
        except:
            print "Unexpected failure while attempting to connect to service"
            print_exc()
            failed = True
        if failed:
            self.process_remove(*k)
                       
    def process_add(self, host, port, service_id, info):
        spec = (host, port, service_id)
        with self.bus.lock:
            if spec in self.services: # Spec is already there, so all we need
                # to do is check and make sure that the info object hasn't
                # changed, and then update the time that we last received
                # something from this service
                if self.services[spec][0] != info: # Info object changed
                    self.services[spec][0] = info
                    self.bus.discover(self, host, port, service_id, info)
                # Update timestamp
                self.services[spec][1] = time.time()
            else: # Spec isn't there, so we need to add it
                self.services[spec] = [info, time.time()]
                self.bus.discover(self, host, port, service_id, info)
    
    def process_remove(self, host, port, service_id):
        spec = (host, port, service_id)
        with self.bus.lock:
            if spec in self.services:
                del self.services[spec]
                self.bus.undiscover(self, host, port, service_id)


class Publisher(object):
    """
    A class capable of publishing information about services available to a
    particular Bus instance.
    
    This class is abstract; one of its subclasses must be used instead. Bus
    instances, by default, install a BroadcastPublisher when they are created.
    """
    @abstract
    def startup(self, bus):
        """
        Called by Bus instances when a publisher is installed onto them. The
        only argument is the bus onto which this publisher was installed.
        """
    
    @abstract
    def shutdown(self):
        """
        Called by Bus instances when a publisher is about to be removed. The
        bus will first call the publisher's remove function for every service
        it had previously added before calling shutdown.
        """
    
    @abstract
    def add(self, service):
        """
        Called to let this publisher know that a new service is available.
        """
    
    @abstract
    def remove(self, service):
        """
        Called to let this publisher know that a service is no longer available.
        """


class BroadcastPublisher(Publisher):
    """
    An implementation of Publisher that uses UDP broadcasts to let others know
    about available services. BroadcastDiscoverer is the matching discoverer
    that knows how to receive these broadcasts.
    """
    def __init__(self):
        self.running = True
        self.services = {}
        self.next_time = 0
    
    def startup(self, bus):
        if not self.running:
            raise Exception("BroadcastPublishers can't be re-used.")
        self.bus = bus
        self.sender, self.receiver = create_broadcast_sockets()
        Thread(name="autobus2.discovery.BroadcastPublisher.receive_loop", target=self.receive_loop).start()
    
    def shutdown(self):
        # print "Shutting down"
        self.running = False
        net.shutdown(self.receiver)
    
    def run_recurring(self):
        if self.next_time < time.time():
            self.next_time = time.time() + (constants.broadcast_interval
                + (random.random() * constants.broadcast_random))
            self.broadcast_services()
            
    def receive_loop(self):
        while True:
            message = None
            try:
                message = self.receiver.recvfrom(16384)[0]
            except SocketError:
                pass
            if not self.running:
                net.shutdown(self.sender)
                return
            self.run_recurring()
            if not message:
                continue
            try:
                message = json.loads(message)
                if message["command"] == "query":
                    self.broadcast_services()
            except:
                print "Couldn't read message in BroadcastPublisher"
                print_exc()
                continue
    
    def respond_to_query(self):
        time.sleep(random.random() * constants.query_response_random)
        self.broadcast_services()
    
    def broadcast_services(self):
        with self.bus.lock:
            for service in self.services.values():
                self.send_add(service)
    
    def send_add(self, service):
        broadcast = {"command": "add", "port": self.bus.port,
                    "service": service.id, "info": service.info}
        broadcast = json.dumps(broadcast)
        net.sendto(self.sender, broadcast, 
                ("127.255.255.255", constants.broadcast_port))
        net.sendto(self.sender, broadcast, 
                ("255.255.255.255", constants.broadcast_port))
    
    def send_remove(self, service):
        broadcast = {"command": "remove", "port": self.bus.port,
                    "service": service.id}
        broadcast = json.dumps(broadcast)
        # Send remove messages in the opposite order of discover messages
        net.sendto(self.sender, broadcast, 
                ("255.255.255.255", constants.broadcast_port))
        net.sendto(self.sender, broadcast, 
                ("127.255.255.255", constants.broadcast_port))

    def add(self, service):
        with self.bus.lock:
            # print "Service added: " + service.id
            self.services[service.id] = service
            self.send_add(service)
        
    
    def remove(self, service):
        with self.bus.lock:
            # print "Service removed: " + service.id
            del self.services[service.id]
            self.send_remove(service)


def create_broadcast_sockets():
    sender = Socket(AF_INET, SOCK_DGRAM)
    sender.setsockopt(SOL_SOCKET, SO_REUSEADDR, True)
    sender.setsockopt(SOL_SOCKET, SO_BROADCAST, True)
    sender.bind(("", 0))
    receiver = Socket(AF_INET, SOCK_DGRAM)
    receiver.settimeout(constants.broadcast_receiver_timeout)
    receiver.setsockopt(SOL_SOCKET, SO_REUSEADDR, True)
    receiver.setsockopt(SOL_SOCKET, SO_BROADCAST, True)
    receiver.bind(("", constants.broadcast_port))
    return sender, receiver































