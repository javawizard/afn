
from libautobus import AutobusConnection
from traceback import print_exc
import sys
import os
try:
    from win32com.client import Dispatch #@UnresolvedImport
except ImportError:
    print_exc()
    print "You need to be on a Windows machine to use activehomed, and you"
    print "need to have pywin32 installed."
    sys.exit()

class RPC(object):
    def action(self, mode, *args):
        args = " ".join(str(arg) for arg in args)
        print "Mode: " + str(mode) + ", command: " + str(args)
        activehome.SendAction(mode, args)
        return "successful"

def main():
    global activehome
    global bus
    try:
        activehome = Dispatch("X10.ActiveHome")
    except:
        print_exc()
        print "You need to install the ActiveHome Scripting SDK before you"
        print "can use activehomed."
        sys.exit()
    print "Connecting to server " + os.getenv("AUTOBUS_SERVER")
    bus = AutobusConnection()
    bus.add_interface("activehome", RPC())
    bus.start_connecting()






















