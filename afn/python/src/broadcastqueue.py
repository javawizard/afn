
from threading import Condition

class Empty(Exception):
    pass


class _Item(object):
    __slots__ = ["value", "link"]
    def __init__(self, value, link):
        self.value = value
        self.link = link


class _Link(object):
    __slots__ = ["item"]
    def __init__(self):
        self.item = None


class BroadcastQueue(object):
    """
    A queue-like object that allows multiple "endpoints", each of which
    provides separate read access to the queue. Whenever an item is added to
    the queue, it becomes available at every endpoint separately.
    """
    def __init__(self):
        self.condition = Condition()
        self._link = _Link()
    
    def put(self, value):
        """
        Inserts an item into this queue.
        """
        with self.condition:
            item = _Item(value, _Link())
            self._link.item = item
            self._link = item.link
    
    def new_endpoint(self):
        """
        Creates a new endpoint for reading from this queue. The endpoint
        initially starts out empty; new items will be available from it once
        Queue.put is called.
        """
        return _Endpoint(self, self._link)


class _Endpoint(object):
    __slots__ = ["_queue", "_link"]
    """
    The class of objects returned from BroadcastQueue.new_endpoint(). Endpoints
    allow reading from a queue.
    """
    def __init__(self, queue, link):
        self._queue = queue
        self._link = link
    
    def get(self):
        """
        Returns the next item available at the endpoint, or raises Empty if no
        items are available.
        
        In the future (read: in a few hours), this will be able to block if no
        items are available.
        """
        with self._queue.condition:
            if self._link.item:
                value = self._link.item.value
                self._link = self._link.item.link
                return value
            else:
                raise Empty







