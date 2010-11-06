
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
        manual_state_change_event(self.number, self.state)
        if self.announce_on_state_change:
            rpc.announce(self.number)
    
    def on_beeping(self):
        print "Timer " + str(self.number) + " is beeping"
        state_change_event(self.number, self.state)
        timer_beeping_event(self.number)
    
    def get_time_fields(self):
        """
        Returns a 3-tuple of the hours, minutes, and seconds, in that order,
        of the timer's absolute value.
        """
        value = self.get_absolute_time()
        return value / 3600, value / 60 % 60, value % 60
    
    def build_object_map(self):
        return {"number": self.number, "time": self.time, "state":
                self.state, "name": self.name, "announce_on_state_change":
        self.announce_on_state_change, "announce_count": self.announce_count,
        "announce_interval": self.announce_interval}

@synchronized(lock)
def publish_timer_object():
    timer_object.set(build_timer_object())

@synchronized(lock)
def build_timer_object():
    result = {}
    for timer in timer_map.values():
        result[timer.number] = timer.build_object_map()
    return result

class RPC(object):
    """
    A timer server. The timer daemon manages timers and provides information
    about those timers. Timers can be counting up, counting down, or stopped,
    which, when passed to functions, are represented by the integers 1, 2, and
    3, respectively. Timers have a notion of a current time showing on the
    timer. The timer daemon keeps track of each timer's current time and
    increments/decrements it automatically if the timer is counting up or
    counting down, respectively.
    
    Integration with the speak daemon is provided. If this Autobus server has
    an interface available named speak when a timer goes off, that interface's
    say_text function will be used to say that the timer is beeping.
    """
    
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
    def get_time(self, timer_number):
        """
        Returns the current absolute time of the specified timer.
        """
        return timer_map[timer_number].get_absolute_time()
    
    @start_thread
    @synchronized(lock)
    def set(self, timer_number, *args):
        """
        This can be called either as set(timer_number, attribute_map) or as
        set(timer_number, attribute_name, new_value). The former set all of the
        attributes in the specified map on the specified timer to the
        specified values. The latter sets a single attribute.
        
        A quick note: to make it easier to set timer state via autosend, a
        timer's state can be one of "up", "down", or "stopped", which will be
        translated by this function to 1, 2, and 3. Normal programs using
        timerd via libautobus should stick with the numeric constants where
        possible, though, as these names may change. This name translation also
        works for the set_attribute function.
        
        An additional note: "stop" is accepted as a synonym for "stopped" in
        the above translation. I just added support for that.
        """
        if len(args) == 1:
            attributes, = args
            cast(attributes, dict)
        else:
            attributes = {args[0]: args[1]}
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
        if "announce_count" in attributes:
            cast(attributes["announce_count"], int, long)
            timer.announce_count = attributes["announce_count"]
        if "state" in attributes:
            if isinstance(attributes["state"], basestring):
                attributes["state"] = {"up": 1, "down": 2, "stop": 3, 
                        "stopped": 3}[attributes["state"]]
            if attributes["state"] != timer.state:
                timer.set_state(attributes["state"])
                timer.on_manual_state_change()
                state_change_event(timer.number, timer.state)
        publish_timer_object()
    
    @start_thread
    @synchronized(lock)
    def set_attribute(self, timer_number, name, value):
        """
        Sets a single attribute. This is equivalent to
        set(timer_number, name, value). It was created back when the set
        function could only be called with a map, and generally shouldn't be
        used anymore.
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
    
    @start_thread
    @synchronized(lock)
    def announce(self, timer_number):
        """
        Announces the time remaining on the specified timer. If speakd is not
        currently running and connected to this Autobus server, this function
        does nothing.
        """
        timer = timer_map[timer_number]
        field_values = timer.get_time_fields()
        field_names = ["hour", "minute", "second"]
        announce_string = ""
        for field_name, field_value in zip(field_names, field_values):
            if field_value != 0:
                announce_string += (" :n:" + str(field_value) + " :pt:" + 
                        field_name + ":s:" + str(field_value))
        if announce_string == "":
            announce_string = " :n:0 " + field_names[-1] + "s"
        if timer.state == UP:
            state_string = "counting up"
        if timer.state == DOWN:
            state_string = "counting down"
        if timer.state == STOPPED:
            state_string = "stopped"
        announce_string = ("timer :n:" + str(timer_number) + " shows " + 
                announce_string + " " + state_string)
        try:
            speak.say_text(announce_string)
        except:
            pass
    
    @start_thread
    @synchronized(lock)
    def dismiss(self, timer_number):
        """
        If the specified timer is currently announcing that it is beeping over
        speakd, this function stops it. Otherwise, this function does nothing.
        """
    
    @start_thread
    @synchronized(lock)
    def list_timers(self):
        """
        Returns a list of the numbers of all timers that currently exist on
        this timer daemon.
        """
        return [timer.number for timer in timer_map.values()]
    
    @start_thread
    @synchronized
    def get_attribute(self, timer_number, attribute_name):
        """
        Returns the current value of the specified attribute on the specified
        timer. This is equivalent to get(timer_number, attribute_name), and
        exists for the same reason that set_attribute exists (and hence
        generally shouldn't be used anymore).
        """
        return self.get(timer_number)[attribute_name]
    
    @start_thread
    @synchronized
    def get(self, timer_number, attribute_name=None):
        """
        This function can be invoked as either get(timer_number) or
        get(timer_number, attribute_name).
        
        When invoked as get(timer_number), returns a map of the specified
        timer's current attributes. Not all of
        these can be set with set_attributes or set; the timer's number, for
        example, cannot be changed. This returns the same map that is
        present in the timers object for the specified timer.
        
        When invoked as get(timer_number, attribute_name), returns the value
        of the specified attribute. This would be equivalent to
        get(timer_number)[attribute_name].
        """
        result = timer_map[timer_number].build_object_map()
        if attribute_name is not None:
            return result[attribute_name]
        else:
            return result

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
    global timer_beeping_event
    global state_change_event
    global manual_state_change_event
    bus = AutobusConnection(host=sys.argv[1] if len(sys.argv) > 1 else None, 
            port=int(sys.argv[2]) if len(sys.argv) > 2 else None,
            print_exceptions=True)
    speak = bus["speak"]
    rpc = RPC()
    bus.add_interface("timer", rpc)
    timer_object = bus.add_object("timer", "timers", "This object is a map of "
            "maps. The outer map maps timer numbers to maps representing "
            "those timers. Each timer's map ", {})
    startup_object = bus.add_object("timer", "startup", "", 0)
    timer_beeping_event = bus.add_event("timer", "beeping", "This event is "
            "fired whenever a timer that is counting down reaches zero and "
            "has its state reset to stopped. It is passed one parameter, "
            "the number of the timer that just went off. The event is fired "
            "before the timer object is updated to reflect that the timer is "
            "now stopped.")
    state_change_event = bus.add_event("timer", "state_change", "This event "
            "is fired whenever a timer changes state, both when a user "
            "manually changes the timer's state and when the state changes "
            "due to the timer reaching zero. This event is passed two "
            "parameters: the number of the timer whose state changed and the "
            "new state of the timer.")
    manual_state_change_event = bus.add_event("timer", "manual_state_change",
            "This event is fired whenever a timer's state is changed "
            "manually by a call to either the set or set_attribute functions. "
            "This event is passed the same set of parameters that "
            "state_change is passed.")
    bus.start_connecting()
    while not is_shut_down:
        sleep(1)
        run_periodic_actions()























