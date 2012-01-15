
from afn.utils import timeutils

def format_time(time):
    """
    Formats a date into a string representing the time of the date that
    can be included in a sentence. For example,
    time_format_time(datetime(hour=15, minute=14)) would
    return ":t:3:14pm".
    """
    hour, minute = time.hour, time.minute
    period = "am" if hour < 12 or hour == 24 else "pm"
    hour = hour % 12
    if hour == 0:
        hour = 12
    return ":t:" + str(hour) + ":" + str(minute).rjust(2, "0") + period

def format_month_day(time):
    """
    Formats a date into a string representing the month and day of the date
    that can be included in a sentence. For example,
    time_format_month_day(datetime(month=3, day=14)) would
    return "march :n:14".
    """
    month = ["january", "february", "march", "april",
             "may", "june", "july", "august",
             "september", "october", "november", "december"
             ][time.month - 1]
    day = str(time.day)
    return month + " :n:" + day

def format_weekday(time):
    """
    Formats a date into a string representing the day of the week that the
    day is on.
    """
    return time.strftime("%A").lower()

def format_weekday_month_day(time):
    """
    Returns time_format_weekday(time) + " " + time_format_month_day(time)
    """
    return (format_weekday(time) + " " + 
            format_month_day(time))
