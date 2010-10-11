
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
    configuration.get("autobus", "port")
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

print "Opening audio device..."

audio_device = pyaudio.PyAudio()
audio_stream = audio_device.open(format=audio_device.get_format_from_width(2),
                                 channels=1, rate=44100, output=True,
                                 frames_per_buffer=1024)
audio_stream.stop_stream()

class Sentence(object):
    def __init__(self, components, voice, priority):
        self.components = components
        self.voice = voice
        self.priority = priority

class Voice(object):
    def __init__(self, name):
        self.name = name
        self.path = configuration.get("voice:" + name, "path")

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
                                    "((?:[AaPp]\\.?[Mm]\\.?)?)",
                                    time_string).groups()
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

def rpc_decode_datetime(time):
    return datetime.strptime(time.value, "%Y%m%dT%H:%M:%S")

class RPC(object):
    def say(self, components, priority=0, voice=None):
        """
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
        """
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
    
    def get_queue_size(self):
        """
        Returns the number of sentences currently in the queue. This does not
        include the sentence that is currently being spoken if there is one.
        """
        return len(list(chain(speech_queue.values())))
    
    def get_voice_names(self):
        """
        Returns a list of the names of all of the voices available to this
        speak server.
        """
        return voices.keys()
    
    def get_default_voice(self):
        """
        Returns the name of the default voice. The default voice is the voice
        used to speak sentences when no voice is specified for the sentence.
        When the speak server starts up, the default voice will be the first
        voice in the configuration file.
        """
        return default_voice
    
    def set_default_voice(self, voice_name):
        """
        Sets the default voice. The default voice is the voice used to speak
        sentences when no voice is specified for the sentence.
        """
        if voice_name not in voices:
            raise Exception("There is no voice with that name. Voices must be "
                            "configured in speakd.conf.")
        default_voice = voice_name
    
    def get_pid(self):
        """
        Returns the pid of the speak server.
        """
        return os.getpid()
    
    def time_format_time(self, time):
        """
        Formats a date into a string representing the time of the date that
        can be included in a sentence. For example,
        time_format_time(datetime(hour=15, minute=14)) would return
        ":t:3:14pm".
        """
        time = rpc_decode_datetime(time)
        hour, minute = time.hour, time.minute
        period = "am" if hour < 12 or hour == 24 else "pm"
        hour = hour % 12
        if hour == 0:
            hour = 12
        return ":t:" + str(hour) + ":" + str(minute).rjust(2, "0") + period
    
    def time_format_month_day(self, time):
        """
        Formats a date into a string representing the month and day of the date
        that can be included in a sentence. For example,
        time_format_month_day(datetime(month=3, day=14)) would return
        "march :n:14".
        """
        time = rpc_decode_datetime(time)
        month = ["january", "february", "march", "april",
                 "may", "june", "july", "august",
                 "september", "october", "november", "december"
                 ][time.month - 1]
        day = str(time.day)
        return month + " :n:" + day
    
    def time_format_weekday(self, time):
        """
        Formats a date into a string representing the day of the week that the
        day is on.
        """
        time = rpc_decode_datetime(time)
        return time.strftime("%A").lower()
    
    def time_format_weekday_month_day(self, time):
        return (self.time_format_weekday(time) + " " + 
                self.time_format_month_day(time))


class RequestHandler(XMLRPCRequestHandler):
    rpc_paths = (rpc_path,)

rpc_server = XMLRPCServer((host, port), RequestHandler, allow_none=True)
rpc_server.request_queue_size = 50
rpc_server.register_introspection_functions()
rpc_server.register_multicall_functions()
rpc_server.register_instance(RPC(), False)

def process_rpc(timeout=0, count=1):
    """
    Processes RPC calls if there are any waiting to be serviced. If
    a timeout is specified, this method will block for that many
    seconds or until an RPC request arrives. count specifies the
    maximum number of RPC calls to service before returning.
    """
    r, w, x = select([rpc_server], [], [], timeout)
    handled = 0
    while rpc_server in r and handled < count:
        handled += 1
        print "Processing RPC call"
        rpc_server._handle_request_noblock()
        r, w, x = select([rpc_server], [], [], 0) # No timeout this time

def sanitize_file(name):
    return name.replace(".", "").replace("\\", "").replace("/", "")

print ""
print "Speakd has successfully started up. PID is " + str(os.getpid())

while not is_shutting_down:
#   print "Main loop"
    sentence_tuple = get_next_sentence()
    if sentence_tuple is None:
#       print "Nothing to say"
        process_rpc(timeout=2.0)
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
            try:
                wave_file = wave.open(os.path.join(voice_path, file + ".wav"), "r")
            except:
                print "File " + file + " does not exist for voice " + voice.name + ". It will be silently ignored."
                continue
            data = wave_file.readframes(1024)
            while data != "":
                audio_stream.write(data)
                process_rpc(count=5)
                data = wave_file.readframes(1024)
        elif isinstance(item, int):
            print "Pausing for " + str(item) + " milliseconds"
            time.sleep((item * 1.0) / 1000.0)
    print "Closing audio stream"
    audio_stream.stop_stream()
    print "Waiting..."
    time.sleep(1)
    print "Done."




















