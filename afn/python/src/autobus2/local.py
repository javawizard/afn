"""
This module contains classes and functions relating to publishing services.
"""

from Queue import Queue, Empty

from autobus2 import net

class RemoteConnection(object):
    """
    A connection from a remote client attached to a particular local service.
    This is for Autobus's use only; external code won't ever need to see this.
    
    The class that external code receives instances of when connecting to a
    service is Connection, not RemoteConnection. When a Connection connects,
    the other end of the socket, the one on the listening end, will be a
    RemoteConnection.
    """
    def __init__(self, bus, socket):
        self.bus = bus
        self.socket = socket
        self.queue = Queue()
        net.OutputThread(socket, self.queue.get).start()
        net.InputThread(socket, self.received, self.cleanup).start()
    
    def received(self, message):
        pass

    def shutdown(self):
        self.queue.put(None)
    
    def cleanup(self):
        self.socket.close()


class LocalService(object):
    """
    A service created locally and published to remote clients. This is the
    class of objects returned from bus.create_service().
    
    The various create_* functions can be called on LocalService instances to
    create functions, events, and objects that are published remotely.
    """
    def __init__(self, bus, id, info):
        self.id = id
        self.info = info
        self.bus = bus
        self.functions = {}
        self.events = {}
        self.objects = {}
    
    def create_function(self):
        pass
    
    def create_event(self):
        pass
    
    def create_object(self):
        pass

