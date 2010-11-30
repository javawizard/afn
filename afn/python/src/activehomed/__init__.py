
from libautobus import AutobusConnection
from traceback import print_exc
import sys
import os
from time import sleep
from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
from urlparse import parse_qsl
try:
    from win32com.client import Dispatch #@UnresolvedImport
    import pythoncom
except ImportError:
    print_exc()
    print "You need to be on a Windows machine to use activehomed, and you"
    print "need to have pywin32 installed."
    sys.exit()

port = 53306

class HTTPHandler(BaseHTTPRequestHandler):
    def do_GET(self):
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
    detected by registering a listener on this interface's receive event.
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
    receive_event = bus.add_event("activehome", "receive", "")
    bus.start_connecting()
    try:
        HTTPServer(("127.0.0.1", port), HTTPHandler).serve_forever()
    except KeyboardInterrupt:
        print "Interrupted, shutting down"
    finally:
        bus.shutdown()
    






















