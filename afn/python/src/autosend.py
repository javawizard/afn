
import sys
from traceback import print_exc
from datetime import datetime # For use with :something arguments
from optparse import OptionParser
from time import sleep

description = """\
A command-line Autobus 1 client. Autosend allows a variety of actions to be
performed against an Autobus server.

(If you want an Autobus 2 command-line client, use autosend2.)

One, and only one, mode option can be specified. This tells autosend what to
do.

Some modes use an interface name. Some use an interface and and item. Some can
also take arguments. Each command-line parameter after the item name is
interpreted as an argument. Arguments are interpreted as follows: if the
argument can be parsed as an integer, it will be sent as an integer. If the
argument is equal to "true", "false", or "null", it will be sent as a boolean
for the former two and as null for the latter. If the argument starts with
a : character, the rest of the argument will be interpreted as a Python
expression whose value will be used as the argument's value. Otherwise, the
argument is interpreted as a string.
"""

parser = OptionParser(usage=
        "usage: autosend [-?] [options] [interface [item [arguments...]]]",
        description="!!!INFO!!!",
        add_help_option=False)
parser.add_option("-h", type="string", action="store", dest="host", help="The host to connect "
        "to. The default is whatever you've configured in libautobus.conf, or "
        "localhost if you haven't created libautobus.conf.", default=None)
from libautobus import DEFAULT_PORT
parser.add_option("-p", type="int", dest="port", help="The port to connect to. "
        "The default is whatever you've configured in libautobus.conf, or "
        + str(DEFAULT_PORT) +
        " if you haven't created libautobus.conf.", default=None)
parser.add_option("-l", action="store_const", const="list", dest="mode",
        help="List mode. No additional command-line arguments are needed. This "
        "is the default mode if no command-line options are specified. In "
        "this mode, Autosend will print out a list of all interfaces currently "
        "registered to the Autobus server, along with their documentation.")
parser.add_option("-i", action="store_const", const="interface", dest="mode",
        help="Interface information mode. Only an interface needs to be "
        "specified; if an item is specified, it will be ignored. This is the "
        "default mode if only an interface is specified. In this mode, "
        "the list of functions, events, and objects present on the specified "
        "remote interface will be printed to stdout along with the documentation "
        "for those items.")
parser.add_option("-f", action="store_const", const="function", dest="mode",
        default="unspecified", help="Function mode. This is the default if an "
        "interface and an item are both specified. This causes the function "
        "specified as the item to be called with the specified arguments. The "
        "function's return value will be printed out.")
parser.add_option("-e", action="store_const", const="event", dest="mode",
        help="Event mode. This attaches to the event specified as the item on "
        "the specified interface. Whenever the event is fired, the event's "
        "arguments will be printed to stdout, followed by a blank line.")
parser.add_option("-o", action="store_const", const="object", dest="mode",
        help="Object mode. This gets the current value of the specified "
        "object and prints it to stdout, then exits.")
parser.add_option("-w", action="store_const", const="watch", dest="mode",
        help="Object watch mode. This gets the current value of the specified "
        "object and prints it to stdout, then waits. Every time the object's "
        "value changes, the new value will be printed to stdout. Like -e, "
        "you'll have to use Ctrl+C to kill autosend when you're done watching "
        "the object's value change.")
parser.add_option("-?", "--help", action="store_const", const="help", dest="mode")
parser.disable_interspersed_args()
options, command_line_args = parser.parse_args()

mode = options.mode
if mode == "unspecified":
    if len(command_line_args) == 0:
        mode = "list"
        print "Use autosend --help for more information.\n"
    elif len(command_line_args) == 1:
        mode = "interface"
    elif len(command_line_args) >= 2:
        mode = "function"

if mode == "help":
    print parser.format_help(None).replace("!!!INFO!!!", description)
    sys.exit()

connect_options = {}
if options.host is not None:
    connect_options["host"] = options.host
if options.port is not None:
    connect_options["port"] = options.port

fixed_values = {"true": True, "false": False, "null": None}
arguments = []

interface_name = None
item_name = None
if len(command_line_args) == 0:
    pass
elif len(command_line_args) == 1:
    interface_name = command_line_args[0]
    command_line_args = []
elif len(command_line_args) >= 2:
    interface_name = command_line_args[0]
    item_name = command_line_args[1]
    command_line_args = command_line_args[2:]

for value in command_line_args:
    # Try parsing as an int
    try:
        arguments.append(int(value))
        continue
    except: # Not an int
        pass
    if value in fixed_values:
        arguments.append(fixed_values[value])
        continue
    if value.startswith(":"):
        arguments.append(eval(value[1:]))
        continue
    arguments.append(value)    
    

from libautobus import AutobusConnection, NotConnectedException
bus = AutobusConnection(**connect_options)

# We've got everything set up thus far. Now we'll look up the mode and figure
# out what we need to do before we connect to the Autobus server.

if mode == "event":
    def event_function(*arguments):
        if len(arguments) < 0:
            print "Function fired with no arguments."
        print str(arguments)
    bus.add_event_listener(interface_name, item_name, event_function)

if mode == "object":
    printed = False
    def object_function(new_value):
        global printed
        if printed:
            return
        printed = True
        print str(type(new_value))
        print str(new_value)
        bus.shutdown()
    bus.add_object_watch(interface_name, item_name, object_function)
elif mode == "watch":
    def object_function(new_value):
        print str(type(new_value))
        print str(new_value)
    bus.add_object_watch(interface_name, item_name, object_function)

try:
    bus.connect()
except NotConnectedException:
    print "Couldn't connect to the Autobus server."
    sys.exit()


if mode == "list":
    try:
        interfaces = bus["autobus"].list_interfaces()
    except Exception as e:
        print "Exception while getting interface list: " + str(e)
        if e.__class__ is not Exception:
            print_exc()
        bus.shutdown()
        sys.exit()
    interfaces.sort(cmp=lambda x, y: cmp(x["name"], y["name"]))
    if len(interfaces) == 0:
        print "No interfaces currently available."
    for interface in interfaces:
        print "Interface " + interface["name"] + ":"
        print interface["doc"]
        print "-" * 79
    bus.shutdown()
elif mode == "interface":
    #TODO: list objects and events as well
    try:
        functions = bus["autobus"].list_functions(interface_name)
        events = bus["autobus"].list_events(interface_name)
        objects = bus["autobus"].list_objects(interface_name)
    except Exception as e:
        print "Exception while getting interface member list: " + str(e)
        if e.__class__ is not Exception:
            print_exc()
        bus.shutdown()
        sys.exit()
    functions.sort(cmp=lambda x, y: cmp(x["name"], y["name"]))
    events.sort(cmp=lambda x, y: cmp(x["name"], y["name"]))
    objects.sort(cmp=lambda x, y: cmp(x["name"], y["name"]))
    def print_object_info(type, items):
        if len(items) == 0:
            print "No %ss currently available on that interface." % type
        for index, item in enumerate(items):
            print "%s%s %s:" % (type[0:1].upper(), type[1:], item["name"])
            print item["doc"]
            if index != (len(items) - 1):
                print "-" * 79
    print_object_info("function", functions)
    print "=" * 79
    print_object_info("event", events)
    print "=" * 79
    print_object_info("object", objects)
    bus.shutdown()
elif mode == "function":
    interface = bus[interface_name]
    function = getattr(interface, item_name)
    try:
        return_value = function(*arguments)
        if return_value is None:
            print "Function ran successfully with no return value."
        else:
            print str(type(return_value))
            print str(return_value)
    except Exception as e:
        if e.__class__ is Exception:
            print "Exception in remote function: " + str(e)
        else:
            print "Local exception:"
            print_exc()
    bus.shutdown()
elif mode == "object" or mode == "watch" or mode == "event":
    # The listeners added before we added will do everything for us, so we just
    # have to wait and watch for keyboard interrupts
    try:
        while not bus.is_shut_down: # Object mode shuts down the bus after
            # it's done
            sleep(0.25)
    except KeyboardInterrupt:
        bus.shutdown()



