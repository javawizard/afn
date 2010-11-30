
from libautobus import AutobusConnection
from traceback import print_exc
import sys
import os
from time import sleep
#from multiprocessing import Process, Queue
try:
    from win32com.client import Dispatch #@UnresolvedImport
    import pythoncom
except ImportError:
    print_exc()
    print "You need to be on a Windows machine to use activehomed, and you"
    print "need to have pywin32 installed."
    sys.exit()

class RPC(object):
    def action(self, mode, *args):
        pythoncom.CoInitialize()
        try:
            args = " ".join(str(arg) for arg in args)
            Dispatch("X10.ActiveHome").SendAction(mode, args)
        finally:
            pythoncom.CoUninitialize()

def main():
    global bus
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
    bus.start_connecting()
    bus.interrupt_loop()






















