
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
    def __init__(self):
        self.condition = Condition()
        self._link = _Link()
    
    def put(self, value):
        with self.condition:
            item = _Item(value, _Link())
            self._link.item = item
            self._link = item.link
    
    def new_endpoint(self):
        pass


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
        with self._queue.condition:
            if self._link.item:
                value = self._link.item.value
                self._link = self._link.item.link
                return value
            else:
                raise Empty







