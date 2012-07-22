"""
This module provides an event loop system.

The blist module must be installed for this to work. It can be obtained with
pip install blist.
"""

from threading import Thread, RLock, current_thread
from Queue import Queue
from bisect import insort
from afn.utils.exceptions import SemanticException


class EventLoop(Thread):
    def __init__(self):
        Thread.__init__(self, target=self._thread_run)
        self._lock = RLock()
        self._queue = Queue()
        # Sequence number that increments whenever a scheduled event is added
        self._sequence = 1
        # Dict of (time, sequence) to (function, (category1, ...))
        self._scheduled = {}
        # List of (time, sequence) in order. Order is maintained by using
        # Python's bisect module; performance is therefore O(n), but my
        # benchmarks have shown removing the first item in a 10,000-item list
        # to take about 15 microseconds, which is acceptable performance for
        # now.
        self._order = []
        # Dict of category names to set([(time1, sequence1), ...]) of the times
        # and sequences in _scheduled
        self._categories = {}
    
    def run(self, function):
        self._queue.put(function)
    
    def run_external(self, function):
        pass
    
    def schedule(self, function, time, *categories):
        # Before we start, make sure we're actually running on the event thread.
        self.ensure_event_thread()
        # First, create a new sequence for this event
        sequence = self._sequence
        self._sequence += 1
        # Then create an id, which is (time, sequence)
        id = time, sequence
        # Then add the event to the _scheduled map, including the function and
        # the event's categories
        self._scheduled[id] = function, categories
        # And then add it to the order list, putting in the correct order with
        # insort (which is a Godsend, by the way).
        insort(self._order, id)
    
    def schedule_external(self, function, time):
        pass
    
    def _thread_run(self):
        pass
    
    def ensure_event_thread(self):
        """
        Raises an exception if this method is called on a thread other than the
        event thread. This more or less checks threading.current_thread(), and
        throws an exception if it doesn't return self.
        """
        if current_thread() is not self:
            raise NotOnEventThread(event_thread=self,
                                   call_thread=current_thread())


class EventLoopException(SemanticException):
    pass


class NotOnEventThread(EventLoopException):
    _format = ("This function can only be called on the event thread "
            "(%(event_thread)r), not on the thread %(call_thread)r.")






