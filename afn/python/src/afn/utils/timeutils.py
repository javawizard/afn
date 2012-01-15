
import datetime as _datetime
import time as _time

def to_time(datetime):
    return _time.mktime(datetime.timetuple())

def to_datetime(time):
    return _datetime.datetime.fromtimestamp(time)