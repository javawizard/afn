
from threading import Condition
from time import time as _time

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
    
    This class provides several advantages over simply tracking a list of
    ordinary Python Queue instances and inserting items into all of them at
    once:
    
        Each endpoint uses a constant amount of space. With an ordinary list of
        queues, 1000 items added to 100 queues would result in 100,000 pointers
        being created, 1000 in each of the 100 queues. Due to the way
        BroadcastQueue is implemented, it would only use 1,100 pointers: one
        for each endpoint and one for each item.
        
        When an endpoint loses all references, any items that have not yet been
        consumed by that endpoint are automatically dereferenced. There's no
        need to remove the endpoint from any sort of list, like you would have
        to do with a list of ordinary Queue instances. Better yet, endpoints
        don't use reference cycles or weak references, so the dereferencing
        happens instantaneously.
    
    An interesting side effect of the automatic dereferencing is that inserting
    into a BroadcastQueue with no endpoints (or only endpoints that have since
    been garbage collected) silently ignores the value to be inserted.
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
            self.condition.notify_all()
    
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
    
    def empty(self):
        """
        Returns True if this endpoint is currently empty, False if it is not.
        
        As usual with queue-like objects, if multiple threads are attempting to
        read from the same endpoint or another thread writes to the endpoint's
        queue right when this function is called, the return value may not be
        entirely accurate.
        """
        # TODO: We probably don't need to lock here
        with self._queue.condition:
            return self._link.item is not None
    
    def peek(self):
        """
        Returns the next item without actually removing it from the endpoint.
        If no such item exists, the Empty exception will be raised.
        """
        # TODO: We probably don't need to lock here either as long as we store
        # self._link.item in a temporary local before checking to see if it's
        # None
        with self._queue.condition:
            if self._link.item:
                return self._link.item.value
            else:
                raise Empty
    
    def get(self, timeout=None):
        """
        Returns the next item available at the endpoint. The behavior of this
        function when an item is not available depends on the timeout
        parameter:
        
            If timeout is 0, the Empty exception is raised immediately.
            
            If timeout is None, this function blocks until an item becomes
            available. The item is then returned.
            
            Otherwise, this function blocks for up to the specified number of
            seconds for an item to become available, raising Empty if an item
            does not become available by the end of the timeout.
        """
        with self._queue.condition:
            # The general structure of this method was shamelessly ripped off
            # from Queue.Queue.get()
            if timeout == 0:
                if not self._link.item:
                    raise Empty
            elif timeout is None:
                while not self._link.item:
                    self._queue.condition.wait()
            elif timeout < 0:
                raise ValueError("'timeout' must be a positive number, not %r"
                        % timeout)
            else:
                end_time = _time() + timeout
                while not self._link.item:
                    remaining = end_time - _time()
                    if remaining <= 0:
                        raise Empty
                    self._queue.condition.wait(remaining)
            value = self._link.item.value
            # Advance to the next item
            self._link = self._link.item.link
            return value







