
from libautobus import AutobusConnection
import sys
import gobject
import gtk

gobject.threads_init()
    
class UserMessageInterface(object):
    def show(self, text):
        pass

def main():
    if len(sys.argv) < 2:
        print "You need to specify the name for this usermessage daemon."
        sys.exit()
    bus = AutobusConnection()
    
    
