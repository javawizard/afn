"""
This module provides an event loop system.

The blist module must be installed for this to work. It can be obtained with
pip install blist.
"""

try:
    from blist import sorteddict
except ImportError:
    print "You don't appear to have the blist module installed. You can"
    print "install it with pip install blist."
from threading import Thread, RLock
from Queue import Queue


class EventLoop(Thread):
    def __init__(self):
        Thread.__init__(self, target=self._thread_run)
        self._lock = RLock()
        self._queue = Queue()
        # Dict of scheduled times to (function, [category1, ...]) tuples
        self._scheduled = sorteddict()
        # Dict of category names to set([time1, time2, ...]) of the times
        # in _scheduled
        self._categories = {}
    
    def run(self, function):
        self._queue.put(function)
    
    def run_external(self, function):
        pass
    
    def schedule(self, function, time, *categories):
        while time in self._scheduled:
            # TODO: Is there a better way to do this? Perhaps store a
            # (time, sequence_number) tuple instead of just time as the key
            time += 0.000001
        self._scheduled[time] = (function, categories)
        # TODO: Add categories to the category map
    
    def schedule_external(self, function, time):
        pass
    
    def _thread_run(self):
        pass







