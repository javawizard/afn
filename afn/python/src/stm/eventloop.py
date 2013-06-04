
from stm import broadcastqueue
import stm
from threading import Thread
import traceback

class EventLoop(object):
    def __init__(self):
        self._queue = broadcastqueue.BroadcastQueue()
        self._endpoint = self._queue.new_endpoint()
    
    def schedule(self, function):
        # In or out of STM
        if function is None:
            raise ValueError("function cannot be None")
        stm.atomically(lambda: self._queue.put(function))
    
    def stop(self):
        # In or out of STM
        stm.atomically(lambda: self._queue.put(None))
    
    def start(self):
        # Outside of STM only
        if stm._stm_state.current:
            raise Exception("This must be called outside of a transaction.")
        thread = Thread(name="stm.eventloop.EventLoop", target=self.run)
        # TODO: Should we be daemonizing here?
        thread.setDaemon(True)
        thread.start()
    
    def run(self):
        # Outside of STM only
        if stm._stm_state.current:
            raise Exception("This must be called outside of a transaction.")
        while True:
            next_event = stm.atomically(self._endpoint.get)
            if next_event is None:
                print "Event loop exiting"
                return
            try:
                next_event()
            except Exception:
                print "Event threw an exception, which will be ignored."
                print "For reference, the exception is:"
                traceback.print_exc()
            except:
                print "Event threw a non-standard exception:"
                traceback.print_exc()
                raise


default_event_loop = EventLoop()
default_event_loop.start()

def schedule(function):
    default_event_loop.schedule(function)





            
