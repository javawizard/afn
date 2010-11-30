
from libautobus import AutobusConnection
from traceback import print_exc
import sys
import os
from time import sleep
from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
from urlparse import parse_qsl
from time import time
from concurrent import synchronized
try:
    from win32com.client import Dispatch #@UnresolvedImport
    import pythoncom
except ImportError:
    print_exc()
    print "You need to be on a Windows machine to use activehomed, and you"
    print "need to have pywin32 installed."
    sys.exit()

port = 53306
RF_DELAY = 0.33
PLC_DELAY = 0.25
last_plc_time = 0.0
last_plc_address = "a1"
last_rf_time = 0.0
last_rf_address = "a1"

class HTTPHandler(BaseHTTPRequestHandler):
    def do_GET(self):
        global last_plc_time
        global last_plc_address
        global last_rf_time
        global last_rf_address
        path = self.path
        if "?" not in path: # Serve the script page
            print "Serving script page for url " + path
            self.send_response(200)
            self.send_header("Content-Type", "text/html")
            self.end_headers()
            with open("src/activehomed/receive_ie.html") as file:
                for line in file:
                    self.wfile.write(line + " ")
        else: # Incoming event from the script page
            print "Inbound event for url " + path
            params = dict(parse_qsl(path.split("?")[1], True))
            mode, address, command, sequence, timestamp, _, _ = (params["a"],
                    params["b"], params["c"], params["d"],
                    params["e"], params["f"], params["g"])
            receive_action_event(mode, address, command, sequence, timestamp)
            repeat = False
            if mode.startswith("send") or mode.startswith("recv"):
                mode = mode[4:]
            now = time()
            if mode == "plc":
                # Command is a repeat if it's within PLC_DELAY seconds of the
                # last command and it's either for the same address as the
                # previous command or the command is one of the
                # address-agnostic commands such as AllLightsOn
                repeat = ((address == last_plc_address
                            or command in ["AllLightsOn", "AllUnitsOff", "AllLightsOff"])
                        and (last_plc_time + PLC_DELAY) > now)
                last_plc_time = now
                last_plc_address = address
            elif mode == "rf":
                repeat = (address == last_rf_address
                        and (last_rf_time + RF_DELAY) > now)
                last_rf_time = now
                last_rf_address = address
            house, unit = address[0], address[1:]
            if sequence == "":
                sequence = None
            else:
                try:
                    sequence = int(sequence)
                except ValueError:
                    print ("WARNING: Sequence number " + str(sequence) +
                            " couldn't be converted to an integer.")
            if timestamp == "":
                timestamp = None 
            receive_event(mode, house, unit, address, command, sequence,
                    timestamp, repeat)
            self.send_response(200)
            self.send_header("Content-Type", "text/plain")
            self.end_headers()
            self.wfile.write("ok")

class RPC(object):
    """
    This interface allows for access to an X10 CM15A module. It allows two-way
    access: powerline and RF commands can be sent, and powerline and RF
    commands can be received. Command lines can be sent with the same syntax
    as the ActiveHome scripting SDK's SendAction method by using this
    interface's action function. Inbound powerline and RF events can be
    detected by registering a listener on this interface's receive and
    receive_action events. (The former should be preferred; it detects repeated
    commands sent by holding down a controller button among other things,
    while the latter is essentially equivalent in signature to the ActiveHome
    scripting SDK's OnRecvAction event, but without the last two parameters
    since the scripting SDK doesn't yet use those.)
    
    One random note: I've noticed that the CM15A seems to receive all powerline
    commands that it sends out, but it doesn't seem to receive any of the RF
    commands that it sends out. Be aware, therefore, that if you send a
    powerline command, the receive and receive_action events will be fired for
    that command, but sending an RF command won't cause the events to be fired.
    """
    def action(self, mode, *args):
        """
        Sends an X10 action. This function has an almost identical signature to
        the SendAction function provided by the ActiveHome scripting SDK's
        ActiveX object. To make it easier on autosend users, all of
        the remaining arguments after the mode (which should be either sendplc
        or sendrf; queryplc should work but I haven't tested it yet) will be
        concatenated with space characters inbetween and the result used as the
        action to perform.
        
        The return value of this function is normally 0. It's most likely going
        to be different if the mode is queryplc or something other than
        sendplc or sendrf; I haven't looked up their documentation yet, though,
        so I don't know precisely what the return values are for those modes.
        """
        pythoncom.CoInitialize()
        try:
            args = " ".join(str(arg) for arg in args)
            return Dispatch("X10.ActiveHome").SendAction(mode, args)
        finally:
            pythoncom.CoUninitialize()

def main():
    global bus
    global receive_event
    global receive_action_event
    pythoncom.CoInitialize()
    try:
        Dispatch("X10.ActiveHome") # Make sure we've got it before hand
    except:
        print_exc()
        print "You need to install the ActiveHome Scripting SDK before you"
        print "can use activehomed."
        sys.exit()
    print "Connecting to server " + str(os.getenv("AUTOBUS_SERVER"))
    bus = AutobusConnection()
    bus.add_interface("activehome", RPC())
    receive_action_event = bus.add_event("activehome", "receive_action", """\
    Fired when the CM15A receives a powerline or RF signal. This event is quite
    low-level and hence the receive event should be preferred to this one. The
    signature of this event is identical to the ActiveHome Scripting SDK's
    OnRecvAction event, except that all parameters are passed as strings and
    the last two parameters are not present (the scripting SDK doesn't make
    use of those at present, so you're not losing any information).
    """)
    receive_event = bus.add_event("activehome", "receive", """\
    receive(mode, house, unit, address, command, sequence, timestamp, repeat)
    
    Fired when the CM15A receives a powerline or RF signal. This event is more
    high-level than receive_action.
    
    mode is the mode, which will either be plc or rf.
    
    house is the house code. unit is the unit code. address is the string
    concatenation of the house and unit codes. For example, house, unit, and
    address could be "d", "2", and "d2", respectively. All three parameters are
    passed as strings.
    
    command is the command that occurred. The ActiveHome Scripting SDK contains
    a list of all valid commands, but some of the more common ones are On, Off,
    AllLightsOn, AllUnitsOff, Bright, Dim, PanLeft, CamGoPosition1, and
    DeviceStop (the latter of which occurs when the CM15A is unplugged from the
    computer).
    
    sequence is slightly command-specific, as follows:
    
    If this is an RF command, sequence will be the sequence number, which
    is 0 for the button being pressed down, 1 and upward for holding
    the button down (fired roughly six times per second), and -1 for
    letting go of the button. A word of warning: this doesn't seem to
    be hugely reliable. For example, pushing an on button on a ScanPad
    or PalmPad results in six events: three pairs of button press and button 
    release commands, despite the fact that the button was only pressed
    once. Holding the button down just repeats these pairs further. If you want
    a reliable indicator of whether or not a button press is a repeat, use the
    repeat parameter passed to this event.
    
    If this is a powerline command, sequence will be null unless the command is
    Bright or Dim, in which case it will be the bright/dim level. When a
    bright/dim button is held down on an RF controller, the result is a
    repeated firing of Bright/Dim commands. When a bright/dim button is held
    down on a powerline controller, the result is that no events are fired
    until the button is let go, and then a single Bright/Dim command is fired
    with the sequence parameter being set to the percent change in brightness
    that would have resulted had the command been directed at an actual module.
    
    The timestamp parameter is only present for RF commands. It specifies, as
    a string, the date and time at which the command was received. This
    generally won't be used since Autobus only incurs a few milliseconds'
    latency, so the date could just be detected locally.
    
    repeat is a boolean. It will be set to true if this event is most likely a
    repeat of another event. For example, if an on button on a ScanPad
    controller is pressed, the result is three pairs of button press and button
    release commands. The first command will have its repeat parameter set to
    false, and the remaining five commands will have their repeat parameters
    set to true. Another example is a bug in the CM15A: If two units were
    simultaneously turned on (for example, by A1 A2 A off going across the
    powerline), and then an AllLightsOn received on the powerline for that
    house code, the CM15A will report that as two events: A1 AllLightsOff and
    A2 AllLightsOff. The latter will be detected as a repeat when this event
    is fired. NOTE: There are currently problems with detecting RF
    bright/dim repeats, so you should generally ignore this parameter for
    RF bright/dim commands.
    """)
    bus.start_connecting()
    try:
        HTTPServer(("127.0.0.1", port), HTTPHandler).serve_forever()
    except KeyboardInterrupt:
        print "Interrupted, shutting down"
    finally:
        bus.shutdown()
    






















