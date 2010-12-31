
from threading import Thread, RLock
from Queue import Queue
from traceback import print_exc

class EventThread(Thread):
    """
    An event dispatch thread. You create one of these by passing in an
    instance of Queue (or any other object that has a no-argument get function)
    and then start it. It reads callable objects off of the queue and calls
    them until the item returned from the queue is None.
    """
    def __init__(self, queue=None):
        """
        Creates a new EventThread with the specified queue. If no queue is
        specified, a new one is created. The queue is then stored in the
        queue attribute on the event thread.
        
        This doesn't start the event thread; you need to remember to call
        start on the event thread to start it.
        """
        if queue is None:
            queue = Queue()
        self.queue = queue
    
    def run(self):
        item = self.queue.get()
        while item is not None:
            try:
                item()
            except:
                print_exc()

class Connection(object):
    pass

class ThreadedServer(Thread):
    pass

class ThreadedConnection(Connection):
    pass
