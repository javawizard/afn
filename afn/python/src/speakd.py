
from ConfigParser import RawConfigParser
from threading import Thread, RLock
import sys
import os
import os.path
import wave
import pyaudio
import random
from SimpleXMLRPCServer import SimpleXMLRPCServer as XMLRPCServer
from SimpleXMLRPCServer import SimpleXMLRPCRequestHandler as XMLRPCRequestHandler
import time
from itertools import chain
import re
from datetime import datetime
from libautobus import AutobusConnection, DEFAULT_PORT
from concurrent import synchronized

lock = RLock()

million = 1000 * 1000
billion = 1000 * 1000 * 1000
trillion = 1000 * 1000 * 1000 * 1000

print "Speakd version 9.0.0.1, starting up..."
print ""
print "    ...what, nine thousand? There's no way that can be right..."
print ""

configuration = RawConfigParser()
print "Loading configuration from speakd.conf..."
try:
    configuration.read("speakd.conf")
    configuration.get("autobus", "host")
except:
    print "You need to create the configuration file speakd.conf before you "
    print "can use speakd. Open speakd.conf.example for an example."
    sys.exit()

host = configuration.get("autobus", "host")
port = configuration.getint("autobus", "port") if configuration.has_option("autobus", "port") else DEFAULT_PORT 
interface_name = configuration.get("autobus", "interface")
voices = {}
default_voice = None
speech_queue = {}
speech_queue_lock = RLock()
is_shutting_down = False

class Sentence(object):
    def __init__(self, components, voice, priority):
        self.components = components
        self.voice = voice
        self.priority = priority

class Voice(object):
    def __init__(self, name):
        self.name = name
        self.path = configuration.get("voice:" + name, "path")

@synchronized(lock)
def get_next_sentence():
    if len(speech_queue) == 0:
        return None
    priority = None
    for current_priority in speech_queue:
        if priority is None or current_priority > priority:
            priority = current_priority
    speech_list = speech_queue[priority]
    result = speech_list[0]
    del speech_list[0]
    if len(speech_list) == 0:
        del speech_queue[priority]
    return result

print "Loading voices..."

for section in configuration.sections():
    if not section.startswith("voice:"):
        continue
    voice_name = section[len("voice:"):]
    voices[voice_name] = Voice(voice_name)
    if default_voice is None:
        default_voice = voice_name

if default_voice is None:
    print "You don't have any voices in your configuration file."
    sys.exit()

print "Starting the RPC server..."

def format_n(number):
    if isinstance(number, basestring):
        number = int(number)
    if number <= 20:
        return str(number)
    if number < 100:
        if (number % 10) == 0:
            return str(number)
        else:
            return str(number / 10 * 10) + " " + str(number % 10)
    if number < 1000:
        if (number % 100) == 0:
            return str(number / 100) + " hundred"
        else:
            return str(number / 100) + " hundred " + format_n(number % 100)
    if number < million:
        if (number % 1000) == 0:
            return format_n(number / 1000) + " thousand"
        else:
            return format_n(number / 1000) + " thousand " + format_n(number % 1000)
    if number < billion:
        if (number % million) == 0:
            return format_n(number / million) + " million"
        else:
            return format_n(number / million) + " million " + format_n(number % million)
    if number < trillion:
        if (number % billion) == 0:
            return format_n(number / billion) + " billion"
        else:
            return format_n(number / billion) + " billion " + format_n(number % billion)
    raise Exception("No such number")

def format_nd(number):
    result = format_n(number)
    suffix = result.split(" ")[-1]
    if suffix == "1":
        return result + "st"
    elif suffix == "2":
        return result + "nd"
    elif suffix == "3":
        return result + "rd"
    return result + "th"

def format_p(number):
    return int(number)

def format_t(time_string):
    hour, minute, period = re.match("([0-9]?[0-9]):([0-9]?[0-9])"
            "((?:[AaPp]\\.?[Mm]\\.?)?)", time_string).groups()
    hour = int(hour)
    minute = int(minute)
    result = format_n(hour).split(" ")
    if minute == 0:
        result.append("o'clock")
    elif minute < 10:
        result += ["oh"] + format_n(minute).split(" ")
    else:
        result += format_n(minute).split(" ")
    if period != "":
        period = period.lower()
        if period[0] == "a":
            result.append("am")
        else:
            result.append("pm")
    return result

class RPC(object):
    """
    Allows communicating with the speak server. This allows pre-recorded
    phrases and synthesized speech to be spoken.
    """
    @synchronized(lock)
    def say(self, components, priority=0, voice=None):
        """
        NOTE: Usually you'll want to use say_text instead of this function.
        That said, here's the documentation...
        
        Adds the specified sentence to the queue. It will be spoken as soon as
        there are no more sentences with a priority higher than this
        sentence's priority and all previously-added sentences with the same
        priority have been spoken. The first parameter, components, is a list
        of strings and ints. Each string represents a single sound to say,
        while each int represents a number of milliseconds to pause before
        continuing in the sentence. Pause gaps can be no longer than 5 seconds.
        The second parameter is an int representing the priority of the
        sentence. Higher-priority sentences will be spoken first. The third,
        and optional, parameter, specifies the name of the voice that should
        be used to say the sentence. If this is null, the sentence will be
        spoken with whatever the default voice is at the time that the speak
        server actually starts to speak the sentence.
        """
        assert isinstance(priority, int)
        if voice is not None and voice not in voices:
            raise Exception("There is no voice with that name. Voices must be "
                            "configured in speakd.conf.")
        if priority not in speech_queue:
            speech_queue[priority] = []
        speech_queue[priority].append((priority, Sentence(components, voice, priority)))
        print "After adding request, speech queue is " + str(speech_queue)
#        speech_queue_object.set(speech_queue)
    
    @synchronized(lock)
    def say_text(self, text, priority=0, voice=None):
        """
        Same as say(text, priority=0, voice=None), but text should be a string,
        and it is parsed into tokens around space characters. If a token starts
        with a : character, then it should be of the format ":type:data", where
        type is a type specifier and data is additional data for that type.
        Valid type specifiers are:
        
        n: The data is a number. This token will be replaced with the words
        needed to say that particular number. Negative numbers are not
        currently allowed. For example, ":n:123" would be replaced with
        "1 hundred 20 3".
        
        nd: The data is a number. This token will be replaced with the words
        needed to state the position of an item by that number. For example,
        ":nd:1" would be replaced with "1st", ":nd:2" would be replaced with
        "2nd", ":nd:123" would be replaced with "1 hundred 20 3rd", and so on.
        
        p: The data is a number. This token causes a pause of the specified
        number of milliseconds to occur.
        
        t: The data is a time, in the format 12:34pm or similar. The period
        specifier can be a.m., p.m., am, or pm, and both uppercase and
        lowercase versions are allowed. 12:34pm would be expanded to
        "12 30 4 pm", 5:03pm (or 05:03pm) would be expanded to "5 oh 3 pm",
        and 7:00pm would be expanded to "7 o'clock pm".
        """
        print "Received request to say text " + str(text) 
        tokens = text.split(" ")
        new_tokens = []
        for token in tokens:
            if token[0] == ":":
                specifier, data = token[1:].split(":", 1)
                format_result = globals()["format_" + specifier](data)
                if isinstance(format_result, basestring):
                    new_tokens += format_result.split(" ")
                elif isinstance(format_result, list):
                    new_tokens += format_result
                elif isinstance(format_result, int):
                    new_tokens.append(format_result)
                else:
                    raise Exception("Invalid format result from format " + 
                                    specifier + " for data " + data + 
                                    ": " + str(format_result))
            else:
                new_tokens.append(token)
        self.say(new_tokens, priority, voice)
    
    @synchronized(lock)
    def get_queue_size(self):
        """
        Returns the number of sentences currently in the queue. This does not
        include the sentence that is currently being spoken if there is one.
        """
        return len(list(chain(speech_queue.values())))
    
    @synchronized(lock)
    def get_voice_names(self):
        """
        Returns a list of the names of all of the voices available to this
        speak server.
        """
        return voices.keys()
    
    @synchronized(lock)
    def get_default_voice(self):
        """
        Returns the name of the default voice. The default voice is the voice
        used to speak sentences when no voice is specified for the sentence.
        When the speak server starts up, the default voice will be the first
        voice in the configuration file.
        """
        return default_voice
    
    @synchronized(lock)
    def set_default_voice(self, voice_name):
        """
        Sets the default voice. The default voice is the voice used to speak
        sentences when no voice is specified for the sentence.
        """
        if voice_name not in voices:
            raise Exception("There is no voice with that name. Voices must be "
                            "configured in speakd.conf.")
        default_voice = voice_name
    
    @synchronized(lock)
    def get_pid(self):
        """
        Returns the pid of the speak server.
        """
        return os.getpid()
    
    @synchronized(lock)
    def time_format_time(self, time):
        """
        Formats a date into a string representing the time of the date that
        can be included in a sentence. For example,
        time_format_time(datetime(hour=15, minute=14)) would return
        ":t:3:14pm".
        """
        hour, minute = time.hour, time.minute
        period = "am" if hour < 12 or hour == 24 else "pm"
        hour = hour % 12
        if hour == 0:
            hour = 12
        return ":t:" + str(hour) + ":" + str(minute).rjust(2, "0") + period
    
    @synchronized(lock)
    def time_format_month_day(self, time):
        """
        Formats a date into a string representing the month and day of the date
        that can be included in a sentence. For example,
        time_format_month_day(datetime(month=3, day=14)) would return
        "march :n:14".
        """
        month = ["january", "february", "march", "april",
                 "may", "june", "july", "august",
                 "september", "october", "november", "december"
                 ][time.month - 1]
        day = str(time.day)
        return month + " :n:" + day
    
    @synchronized(lock)
    def time_format_weekday(self, time):
        """
        Formats a date into a string representing the day of the week that the
        day is on.
        """
        return time.strftime("%A").lower()
    
    @synchronized(lock)
    def time_format_weekday_month_day(self, time):
        """
        Returns time_format_weekday(time) + " " + time_format_month_day(time)
        """
        return (self.time_format_weekday(time) + " " + 
                self.time_format_month_day(time))


def sanitize_file(name):
    return name.replace(".", "").replace("\\", "").replace("/", "")

    print "Opening audio device..."

def main():
    global audio_device
    global autio_stream
    global bus
    global speech_queue_object
    audio_device = pyaudio.PyAudio()
    audio_stream = audio_device.open(format=audio_device.get_format_from_width(2),
                                     channels=1, rate=44100, output=True,
                                     frames_per_buffer=1024)
    audio_stream.stop_stream()
    
    bus = AutobusConnection(host, port, print_exceptions=True)
    bus.add_interface(interface_name, RPC())
    speech_queue_object = bus.add_object(interface_name, "speech_queue", """
    This doesn't work yet.
    """, {})
    bus.start_connecting()
    
    print ""
    print "Speakd has successfully started up. PID is " + str(os.getpid())
    try:
        while not is_shutting_down:
        #   print "Main loop"
            sentence_tuple = get_next_sentence()
            if sentence_tuple is None:
        #       print "Nothing to say"
                time.sleep(0.5)
                continue
            priority, sentence = sentence_tuple
            print "Something to say: " + str(sentence.components)
            if sentence.voice is not None:
                voice = voices[sentence.voice]
            else:
                voice = voices[default_voice]
            voice_path = voice.path
            print "Saying with voice " + voice.name
            audio_stream.start_stream()
            for item in sentence.components:
                if isinstance(item, basestring):
                    file = sanitize_file(item)
                    print "Saying word " + file
                    absolute_file = os.path.join(voice_path, file + ".wav")
                    try:
                        wave_file = wave.open(absolute_file, "r")
                    except:
                        print "File " + absolute_file + " does not exist for voice " + voice.name + ". It will be silently ignored."
                        continue
                    data = wave_file.readframes(1024)
                    while data != "":
                        audio_stream.write(data)
                        data = wave_file.readframes(1024)
                elif isinstance(item, int):
                    print "Pausing for " + str(item) + " milliseconds"
                    time.sleep((item * 1.0) / 1000.0)
                print "Flushing bus outbound queue"
                bus.flush()
            print "Waiting..."
            time.sleep(1)
            print "Closing audio stream"
            audio_stream.stop_stream()
            print "Done."
    except KeyboardInterrupt:
        print "Interrupted, shutting down"
        bus.shutdown()



















