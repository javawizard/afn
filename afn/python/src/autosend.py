
import sys
from traceback import print_exc
from datetime import datetime # For use with :something arguments

if len(sys.argv) < 3:
    print "Usage: autosend <interface-name> <function-name> <arguments...>"
    print "Connects to Autobus (on the default host and port set in"
    print "libautobus.conf) and calls the specified function on the specified"
    print "interface with the specified arguments. Each parameter after the"
    print "function name is interpreted as an argument to pass to the"
    print "function. If the parameter can be parsed as an integer, it will be"
    print 'sent as such. If it is equal to "true", "false", or "null", it will'
    print "be sent as the corresponding value. Otherwise, it will be sent as"
    print "a string. If different behavior is desired, the parameter can be"
    print "prefixed with a : character, and the rest of the parameter is"
    print "interpreted as a python expression whose value will be used as the"
    print "parameter to send."
    print ""
    print "The first line printed will be the type of the result. The second"
    print "line will be the result itself. If the function threw an exception,"
    print "the first line will be the exception type and the second line and"
    print "onward will be the traceback."
    sys.exit()

fixed_values = {"true": True, "false": False, "null": None}
arguments = []

for value in sys.argv[3:]:
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

from libautobus import AutobusConnection
bus = AutobusConnection()
bus.connect()

interface = bus[sys.argv[1]]
function = getattr(interface, sys.argv[2])

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



