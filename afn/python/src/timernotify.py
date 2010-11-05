
import sys
from libautobus import AutobusConnection
from optparse import OptionParser
from time import sleep

def main():
    try:
        import pynotify
    except ImportError:
        print "You don't have the Python libnotify binding installed."
        sys.exit()
    
    if not pynotify.init("timernotify"):
        print "Libnotify could not be initialized."
        sys.exit()
    
    parser = OptionParser(usage="usage: python -m timernotify [options]", 
            add_help_option=False)
    parser.add_option("-h", type="string", action="store", dest="host", default="localhost",
            help="The host that the Autobus server is running on. Without this "
            "option, localhost will be used.")
    parser.add_option("-s", action="store_const", const=True, dest="state_change",
            default=False, help="If present, all state changes (instead of just "
            "when the timer beeps) will cause a notification to be shown.")
    parser.add_option("-?", "--help", action="help")
    
    options, command_line_args = parser.parse_args()
    
    bus = AutobusConnection(host=options.host)
    def beeping_listener(timer):
        pynotify.Notification("Timer " + str(timer), "is beeping.").show()
    
    def state_change_listener(timer, state):
        state_string = {1: "counting up", 2: "counting down", 3: "stopped"}[state]
        pynotify.Notification("Timer " + str(timer), "is now " + state_string + 
                ".").show()
    
    bus.add_event_listener("timer", "beeping", beeping_listener)
    if options.state_change:
        bus.add_event_listener("timer", "manual_state_change", state_change_listener)
    
    bus.connect()
    
    try:
        while True:
            sleep(1)
    except KeyboardInterrupt:
        print "Interrupted, shutting down"
    finally:
        bus.shutdown()














