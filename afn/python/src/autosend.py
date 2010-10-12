
import sys
from traceback import print_exc
from datetime import datetime # For use with :something arguments
from optparse import OptionParser

description = """\
A command-line Autobus client. Autosend allows a variety of actions to be
performed against an Autobus server.

If an interface is not specified, Autosend prints out a list of interfaces
currently available on the Autobus server, along with some information about
each of them. If an interface is specified but a function is not, Autosend
prints out information about all of the functions available on the interface.
If both an interface and a function are specified. Autosend invokes the
specified function and prints out its return value or the exception it raised,
if any.

The arguments, if any, specified after the function's name are arguments to
supply to the function. Each command-line argument is an argument to pass into
the function. If the argument is equal to "true", "false", or "null", the
corresponding values will be passed into the function. If the argument can be
parsed as an integer, it will be passed in as such. If it starts with
a : character, the rest of the argument will be evaluated as a Python
expression and the result passed into the function. Otherwise, the argument is
passed into the function as a string.

Autosend currently does not have support for performing actions on other types
of data, such as events or objects. Such support will most likely be added in
the future.
"""

parser = OptionParser(usage=
        "usage: autosend [options] [interface [function [arguments...]]]",
        description="!!!INFO!!!",
        add_help_option=False)
parser.add_option("-h", type="string", dest="host", help="The host to connect "
        "to. The default is whatever you've configured in libautobus.conf, or "
        "localhost if you haven't created libautobus.conf.", default=None)
parser.add_option("-p", type="int", dest="port", help="The port to connect to. "
        "The default is whatever you've configured in libautobus.conf, or 28862 "
        "if you haven't created libautobus.conf.", default=None)
parser.add_option("-?", "--help", action="store_true", dest="help", default=False)
options, command_line_args = parser.parse_args()
if options.help:
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
function_name = None
if len(command_line_args) == 0:
    pass
elif len(command_line_args) == 1:
    interface_name = command_line_args[0]
    command_line_args = []
elif len(command_line_args) >= 2:
    interface_name = command_line_args[0]
    function_name = command_line_args[1]
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

from libautobus import AutobusConnection
bus = AutobusConnection(*connect_options)
bus.connect()


if interface_name is None:
    interfaces = bus["autobus"].list_interfaces()
    interfaces.sort(cmp=lambda x, y: cmp(x["name"], y["name"]))
    if len(interfaces) == 0:
        print "No interfaces currently available."
    for interface in interfaces:
        print "Interface " + interface["name"] + ":"
        print interface["doc"]
        print "---------------------------------------------------------------"
    bus.shutdown()
    sys.exit()

if function_name is None:
    functions = bus["autobus"].list_functions(interface_name)
    functions.sort(cmp=lambda x, y: cmp(x["name"], y["name"]))
    if len(functions) == 0:
        print "No functions currently available on that interfacce."
    for function in functions:
        print "Function " + function["name"] + ":"
        print function["doc"]
        print "---------------------------------------------------------------"
    bus.shutdown()
    sys.exit()

interface = bus[interface_name]
function = getattr(interface, function_name)

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



