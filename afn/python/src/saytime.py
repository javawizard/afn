
from datetime import datetime, timedelta as interval
from time import sleep
from xmlrpclib import ServerProxy
import sys

speak_server = ServerProxy(sys.argv[1])

start_time = datetime.now()
start_minute_add = 1
if start_time.second > 57:
    start_minute_add = 2
next_minute = datetime(year=start_time.year, month=start_time.month, day=start_time.day, hour=start_time.hour, minute=start_time.minute + start_minute_add)

while True:
    time_to_next = (next_minute - datetime.now()).seconds
    print "Waiting " + str(time_to_next) + " seconds"
    sleep(time_to_next)
    current_hour = next_minute.hour
    current_minute = next_minute.minute
    current_time = next_minute
    next_minute = next_minute + interval(seconds=70)
    next_minute = datetime(year=next_minute.year, month=next_minute.month, day=next_minute.day, hour=next_minute.hour, minute=next_minute.minute)
    print "Processing instructions for " + str(current_hour) + ":" + str(current_minute)
    if (current_hour >= 8 and current_hour < 20) or (current_hour == 20 and 
                                                     current_minute == 0): 
        if current_minute == 0:
            speak_server.say_text("today is " + 
                    speak_server.time_format_weekday_month_day(current_time) +
                    " :p:600 the_time is " + 
                    speak_server.time_format_time(current_time))
        elif (current_minute % 15) == 0:
            speak_server.say_text("the_time is " + 
                                  speak_server.time_format_time(current_time)) 














