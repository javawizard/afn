
from threading import Thread
from Queue import Queue, Empty
from traceback import print_exc

class TkThread(object):
    """
    This class starts a thread that runs a Tkinter mainloop(). It allows UI
    tasks to be submitted to it, which it will execute as fast as it can and
    in order. The return value from those tasks can be captured if needed. This
    makes it a bit easier for multithreaded applications to use Tkinter.
    """
    def __init__(self, init_function):
        """
        Creates a new TkThread. init_function is a function that, when called,
        should create the root Tk window and return it.
        
        After you create a new TkThread, you need to call its start function
        to actually get things going. The thread will then call the init
        function
        """
        self.init_function = init_function
        self.queue = Queue()
    
    def run(self):
        toplevel = self.init_function()
        self.toplevel = toplevel
        toplevel.after(100, self._process_events)
        toplevel.mainloop()
    
    def _process_events(self):
        try:
            while True:
                event = self.queue.get_nowait()
                try:
                    event()
                except:
                    print_exc()
        except Empty:
            pass
        finally:
            self.toplevel.after(100, self._process_events)
    
    def call(self, function, *args):
        """
        Schedules the specified function to be called on the Tkinter thread.
        """
        self.queue.put(lambda: function(*args))
    
    def call_sync(self, function, *args):
        """
        Schedules the specified function to be called on the Tkinter thread,
        then waits until it gets called and returns its return value.
        """
        wait_queue = Queue()
        def event():
            try:
                value = (False, function(*args))
            except Exception, e:
                value = (True, e)
            wait_queue.put(value)
        self.call(event)
        excepted, value = wait_queue.get()
        if excepted:
            raise value
        else:
            return value







