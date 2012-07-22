"""
This module provides an event loop system.

The blist module must be installed for this to work. It can be obtained with
pip install blist.
"""

from threading import Thread, RLock, current_thread
from Queue import Queue, Empty
from bisect import insort
from afn.utils.exceptions import SemanticException
from afn.utils import Suppress, print_exceptions
import time
import functools
from afn.utils.partial import Partial


class EventLoop(Thread):
    def __init__(self):
        Thread.__init__(self, target=self._thread_run)
        self._lock = RLock()
        self._queue = Queue()
        # This is set to False to shut down the event loop.
        self.alive = True
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
        """
        Adds the specified function to the event queue. It will be run as soon
        as all events scheduled before it have finished running.
        """
        self._queue.put(function)
    
    def on(self, function):
        """
        A function decorator that causes any invocations of the decorated
        function to be run on this event loop. The function call will return
        None, and the actual function will be scheduled to run.
        
        This can be used thus:
        
        loop = EventLoop()
        @loop.on
        def test(arg1, arg2, ...):
            ...do something with arg1, arg2, etc...
        
        From there on out, any calls to test will return None, and the function
        will be scheduled to execute on the specified event loop.
        """
        @functools.wraps(function)
        def wrapper(*args, **kwargs):
            # Schedule the function to be called with the specified args on the
            # event loop
            self.run(Partial(function, *args, **kwargs))
            # Then we just drop off the end of the function to return None.
        # Return the wrapper that will cause the function to be run on the
        # event loop.
        return wrapper
    
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
        # Last of all, we add the id to the relevant categories.
        for category in categories:
            self._categories.setdefault(category, []).append(category)
        # And we're done!
    
    def cancel(self, *categories):
        """
        Cancels all scheduled events registered under any of the specified
        categories.
        """
        self.ensure_event_thread()
        # Look up the relevant categories and remove their entries from the
        # category dictionary
        ids = set()
        for category in categories:
            ids |= set(self._categories.get(category, []))
            with Suppress(KeyError):
                del self._categories[category]
        # We've nixed the categories, and we've got the ids. Now remove the ids
        # from _order and _scheduled.
        for id in ids:
            del self._scheduled[id]
            self._order.remove(id)
    
    def _thread_run(self):
        while self.alive:
            # See if there are any scheduled events past due
            if self._order and self._order[0][0] < time.time():
                # There is a scheduled event past due
                id = self._order[0]
                function, categories = self._scheduled[id]
                # Remove the event from _order and _scheduled
                del self._scheduled[id]
                self._order.remove(id)
                # Remove the event from its categories
                for category in categories:
                    self._categories[category].remove(id)
                    # If there aren't any more events with that category,
                    # remove the category.
                    if not self._categories[category]:
                        del self._categories[category]
                # Then run the event.
                with print_exceptions:
                    function()
            # We've either run a scheduled event or we haven't. Now we check to
            # see if there are any more scheduled events, and we calculate the
            # timeout to be the time until the next scheduled event is to run.
            if self._order:
                # Use max to ensure we sleep 0 seconds if the next event is
                # due in the past
                timeout = max(0, self._order[0][0] - time.time())
            else: # No scheduled events, so wait indefinitely
                timeout = None
            # Now wait for an immediate event to come through, or at most the
            # number of seconds until the next scheduled event.
            try:
                # Block if timeout is None or a positive number; don't block
                # if timeout is 0, as that means we've got a paste-due event.
                # I've no clue why Queue.get actually takes a block parameter,
                # as timeout=0 would suffice to indicate non-blocking behavior.
                function = self._queue.get(timeout != 0, timeout)
                # We got a function, so run it.
                with print_exceptions:
                    function()
            except Empty: # Timeout happened, so we just return
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
    
    def shutdown(self):
        """
        Adds an event to the event queue that will shut down the event loop
        when it runs. This can be called from any thread.
        
        If you want to immediately stop the event queue from a function running
        on the event queue itself, use self.alive = False instead.
        """
        def stop():
            self.alive = False
        self.run(stop)


class EventLoopException(SemanticException):
    pass


class NotOnEventThread(EventLoopException):
    _format = ("This function can only be called on the event thread "
            "(%(event_thread)r), not on the thread %(call_thread)r.")






