
from libautobus import AutobusConnection
from traceback import print_exc
import sys
try:
    from win32com.client import Dispatch #@UnresolvedImport
except ImportError:
    print_exc()
    print "You need to be on a Windows machine to use activehomed, and you"
    print "need to have pywin32 installed."
    sys.exit()

def main():
    global activehome
    try:
        activehome = Dispatch("X10.ActiveHome")
    except:
        print_exc()
        print "You need to install the ActiveHome Scripting SDK before you"
        print "can use activehomed."
        sys.exit()