"""
A queue with multiple endpoints from which items can be consumed.

See the documentation for the BroadcastQueue class for more information.
"""

from stm_system import stm
from stm_system.datatypes import tobject

class Empty(Exception):
    """
    Exception thrown from endpoints' get() functions when block=False is passed
    in and no items are currently available.
    """
    pass


class _Item(object):
    """
    An item that has been added to the queue. Items themselves are immutable,
    but they contain a TVar pointing to the next item added to the queue, or
    None if a "next item" hasn't been added yet.
    """
    def __init__(self, value):
        self.value = value
        self.next = stm.TVar()


class BroadcastQueue(tobject.TObject):
    """
    A queue that can have multiple endpoints from which items can be consumed.
    
    Endpoints are created by calling new_endpoint(). Each endpoint initially
    starts out empty; items become available as soon as the creating queue's
    put() function is called.
    
    Items inserted into the queue with put() become available on all endpoints
    to consume. This allows BroadcastQueues to be used to broadcast values to
    several different consumers.
    
    When an endpoint is no longer needed, it can be simply discarded. Endpoints
    hold a reference to the queue they were created from, not the other way
    around, so they will be immediately garbage collected and any items unread
    by the endpoint but not by any other endpoint immediately reclaimed.
    
    An interesting side effect of this is that adding items to a queue that has
    never had any endpoints created from it, or one that has had all of its
    endpoints discarded, silently discards the items added to it, and is thus
    a no-op.
    """
    def __init__(self):
        tobject.TObject.__init__(self)
        self._var = stm.TVar(None)
    
    def put(self, value):
        """
        Inserts an item into this queue. The item will then become available on
        all endpoints created from it.
        """
        item = _Item(value)
        self._var.set(item)
        self._var = item.next
    
    def new_endpoint(self):
        """
        Creates a new endpoint that receives values added to this queue. The
        endpoint initially starts out empty; items will appear on it as soon as
        put() is called next.
        """
        return BroadcastEndpoint(self._var)


class BroadcastEndpoint(tobject.TObject):
    """
    A broadcast endpoint from which items can be read.
    
    Normally, you won't need to create instances of this class yourself;
    they're typically obtained by calling new_endpoint on a BroadcastQueue
    instance.
    """
    def __init__(self, var):
        tobject.TObject.__init__(self)
        self._var = var
    
    def get(self, block=True):
        """
        Removes and returns the next available item from this endpoint.
        
        If block is True and there aren't any items currently available on this
        endpoint, this function retries. If block is False, the Empty exception
        is raised instead.
        """
        if self._var.get() is None:
            if block:
                stm.retry()
            else:
                raise Empty
        else:
            item = self._var.get()
            self._var = item.next
            return item.value
        
