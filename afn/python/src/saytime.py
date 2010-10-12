
from datetime import datetime, timedelta as interval
from time import sleep
from libautobus import AutobusConnection
import sys
from concurrent import as_new_thread

class RPC(object):
    """
    Provides functions for saying the current time. This program says the time
    automatically every 15 minutes as well. There isn't yet a way to configure
    this automatic time-saying from Autobus, but I'll be adding that soon. 
    """
    @as_new_thread
    def time(self):
        """
        Instructs the speak server to say the current time.
        """
        speak_server.say_text("the_time is " + 
                speak_server.time_format_time(current_time))
    
    @as_new_thread
    def datetime(self):
        """
        Instructs the speak server to say the current date and time.
        """
        speak_server.say_text("today is " + 
                speak_server.time_format_weekday_month_day(current_time) + 
                " :p:600 the_time is " + 
                speak_server.time_format_time(current_time))
    
    def current_time(self):
        """
        Returns the current minute according to the server. This is the time
        that would be spoken had datetime() or time() been called instead of
        this function.
        """
        return current_time

rpc = RPC()

bus = AutobusConnection()
bus.add_interface("saytime", rpc)
bus.connect()
speak_server = bus["speak"]

start_time = datetime.now()
current_time = start_time
next_minute = start_time

try:
    while True:
        interval_to_next = next_minute - datetime.now()
        time_to_next = float(interval_to_next.seconds) + (interval_to_next.microseconds / 1000000.0)
        if interval_to_next.days < 0:
            time_to_next = 0
        print "Waiting " + str(time_to_next) + " seconds"
        sleep(time_to_next)
        sleep(0.2) # Make sure we're at the next minute
        current_hour = next_minute.hour
        current_minute = next_minute.minute
        current_time = next_minute
        next_minute = next_minute + interval(seconds=70)
        next_minute = datetime(year=next_minute.year, month=next_minute.month, day=next_minute.day, hour=next_minute.hour, minute=next_minute.minute)
        print "Processing instructions for " + str(current_hour) + ":" + str(current_minute)
        if (current_hour >= 8 and current_hour < 20) or (current_hour == 20 and 
                                                         current_minute == 0): 
            if current_minute == 0:
                rpc.datetime()
            elif (current_minute % 15) == 0:
                rpc.time()
finally:
    bus.shutdown()














