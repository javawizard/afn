
from libautobus import AutobusConnection, start_thread
from threading import Thread, RLock
from datetime import datetime
from time import sleep
import sys
from concurrent import synchronized
from utils import cast

UP = 1
DOWN = 2
STOPPED = 3
STATES = [UP, DOWN, STOPPED]

lock = RLock()
time_since_startup = 0
startup_time = datetime.now()
timer_map = {} # Maps timer numbers to timer instances
is_shut_down = False

class Timer(object):
    def __init__(self, number):
        """
        Creates a new timer. This does not add it to the timer map.
        """
        self.number = number
        self.time = 0
        self.state = STOPPED
        self.announce_interval = 0
        self.name = ""
        self.announce_count = 5
        self.announce_on_state_change = True
    
    def get_absolute_time(self):
        """
        Calculates the absolute time remaining on this timer. If this timer's
        state is stopped, this returns self.time. If this timer's state is
        counting up, this returns time_since_startup - self.time. If this
        timer's state is counting down, this returns
        self.time - time_since_startup.
        """
        if self.state == STOPPED:
            return self.time
        if self.state == UP:
            return time_since_startup - self.time
        if self.state == DOWN:
            return self.time - time_since_startup
    
    def set_absolute_time(self, new_time):
        """
        Sets the absolute time on this timer to the specified value.
        """
        if self.state == STOPPED:
            self.time = new_time
        if self.state == UP:
            self.time = time_since_startup - new_time
        if self.state == DOWN:
            self.time = time_since_startup + new_time
    
    def set_state(self, new_state):
        """
        Sets this timer's state to the specified state. The timer's time will
        be modified as needed to fit the new state.
        """
        time = self.get_absolute_time()
        self.state = new_state
        self.set_absolute_time(time)
    
    def on_manual_state_change(self):
        print ("Manual state change on timer " + str(self.number) +
                " with state set to " + str(self.state))
    
    def on_beeping(self):
        print "Timer " + str(self.number) + " is beeping"

@synchronized(lock)
def publish_timer_object():
    timer_object.set(build_timer_object())

@synchronized(lock)
def build_timer_object():
    result = {}
    for timer in timer_map.values():
        timer_info = {"number": timer.number, "time": timer.time, "state":
                timer.state, "name": timer.name}
        result[timer.number] = timer_info
    return result

class RPC(object):
    
    @start_thread
    @synchronized(lock)
    def create(self):
        """
        Creates a new timer. This function returns the number of the timer
        just created.
        """
        timer_number = 1
        while timer_number in timer_map:
            timer_number += 1
        timer = Timer(timer_number)
        timer_map[timer_number] = timer
        publish_timer_object()
        return timer_number
    
    @start_thread
    @synchronized(lock)
    def set_time(self, timer_number, new_absolute_time):
        """
        Sets the specified timer's absolute time to the specified time.
        """
        timer_map[timer_number].set_absolute_time(new_absolute_time)
        publish_timer_object()
    
    @start_thread
    @synchronized(lock)
    def update_time(self, timer_number, amount_to_add):
        """
        Adds the specified amount to the timer's absolute value. The value can
        be negative.
        """
        timer_map[timer_number].set_absolute_time(
                timer_map[timer_number].get_absolute_time() + amount_to_add)
        publish_timer_object()
    
    @start_thread
    @synchronized(lock)
    def set(self, timer_number, attributes):
        """
        Sets the attributes present in the specified map on the specified
        timer.
        """
        timer = timer_map[timer_number]
        if "announce_interval" in attributes:
            cast(attributes["announce_interval"], int, long)
            timer.announce_interval = attributes["announce_interval"]
        if "name" in attributes:
            cast(attributes["name"], basestring)
            timer.name = attributes["name"]
        if "announce_on_state_change" in attributes:
            cast(attributes["announce_on_state_change"], bool)
            timer.announce_on_state_change = attributes["announce_on_state_change"]
        if "state" in attributes:
            if attributes["state"] != timer.state:
                timer.set_state(attributes["state"])
                timer.on_manual_state_change()
        publish_timer_object()
    
    @start_thread
    @synchronized(lock)
    def set_attribute(self, timer_number, name, value):
        """
        Sets a single attribute. This is equivalent to
        set(timer_number, {name: value}). It exists mostly to make setting
        timer attributes with autosend easier than it would otherwise be.
        """
        self.set(timer_number, {name: value})
    
    @start_thread
    @synchronized(lock)
    def delete(self, timer_number):
        """
        Deletes the timer with the specified number. An exception will be
        thrown if the specified timer does not exist.
        """
        if timer_number not in timer_map:
            raise Exception("There is no timer with the number " + str(timer_number))
        del timer_map[timer_number]
        publish_timer_object()

@synchronized(lock)
def run_periodic_actions():
    """
    Runs tasks that should be run once per second.
    """
    global time_since_startup
    now = datetime.now()
    startup_interval = now - startup_time
    time_since_startup = startup_interval.seconds + (startup_interval.days * 86400)
    startup_object.set(time_since_startup)
    print "Startup time: " + str(time_since_startup)
    modified = False
    for timer_number in timer_map:
        timer = timer_map[timer_number]
        time = timer.get_absolute_time()
        if timer.state == DOWN and time <= 0:
            modified = True
            time = 0
            timer.set_absolute_time(0)
            timer.set_state(STOPPED)
            timer.on_beeping()
        if time < 0:
            modified = True
            time = 0
            timer.set_absolute_time(0)
    if modified:
        publish_timer_object()

class IntervalThread(Thread):
    """
    A thread that checks the timers every second and performs any actions that
    need to be performed on them.
    """
    def run(self):
        while not is_shut_down:
            sleep(1)
            run_periodic_actions()

def main():
    global is_shut_down
    try:
        run()
    except KeyboardInterrupt:
        print "Interrupted, shutting down"
    finally:
        bus.shutdown()
        is_shut_down = True

def run():
    global bus
    global speak
    global rpc
    global timer_object
    global startup_object
    if len(sys.argv) > 1:
        bus = AutobusConnection(host=sys.argv[1])
    else:
        bus = AutobusConnection()
    speak = bus["speak"]
    rpc = RPC()
    bus.add_interface("timer", rpc)
    timer_object = bus.add_object("timer", "timers", "", {})
    startup_object = bus.add_object("timer", "startup", "", 0)
    bus.start_connecting()
    while not is_shut_down:
        sleep(1)
        run_periodic_actions()























