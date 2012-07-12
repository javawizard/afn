
try:
    import argparse
except ImportError:
    from afn.backports import argparse
from functools import partial
from autobus2 import Bus, wait_for_interrupt
import autobus2
import time
import json
import platform
from afn.utils.concurrent import dump_thread_traces
# Import signal on Linux only
if platform.system() == "Linux":
    import signal
else:
    signal = None

description = """
A command-line Autobus client.
"""

parser = argparse.ArgumentParser(prog="autosend2", description="!!!INFO!!!", add_help=False)

parser.add_argument("type", nargs="?")
parser.add_argument("name", nargs="?")
parser.add_argument("values", nargs="*", metavar="value")

modes = parser.add_argument_group("modes").add_mutually_exclusive_group()
add_mode = partial(modes.add_argument, action="store_const", dest="mode")
add_mode("-f", "--call", const="call",
        help="Function call mode")
add_mode("-b", "--broadcast", const="broadcast",
        help="Broadcast watch mode")
add_mode("-d", "--discovery", const="discovery",
        help="Discovery mode. In this mode, autosend2 prints a table of all "
        "services available on the network, and then prints a message whenever "
        "a service appears or disappears.")
add_mode("-l", "--list", const="list",
        help="List mode. In this mode, autosend2 prints out information about "
        "all matching services, including their documentation and a list of "
        "all functions, events, etc on the service and their documentation.")
add_mode("-?", "--help", const="help",
        help="Show this help message")

options = parser.add_argument_group("optional arguments")
options.add_argument("-k", "--filter-equal", action="append", dest="filter_equal",
        nargs=2, metavar=("key", "value"), default=[],
        help="The specified key must have the specified value.")
options.add_argument("-m", "--multiple", action="store_true", help=
        "Causes commands such as --call and --list to try to connect to as "
        "many services as possible. Those commands normally connect to the "
        "first service that they can; with --multiple, they will wait up to "
        "however long is specified by -t (which defaults to 2 seconds) and "
        "try to connect to as many services as they can during that time.")
options.add_argument("-t", "--time", action="store", type=int, default=2, help=
        "Specifies the amount of time that autosend2 should try to connect to "
        "services for when -m is used. This has no effect when -m is not used.")
options.add_argument("-a", "--all", action="store_true", help=
        "Normally, list mode, among others, filter internal functions, such as "
        '"autobus.get_details", that are usually supposed to stay hidden. '
        "Specifying --all shows these hidden functions.")

def main():
    try:
        # I've been seeing a problem lately where autosend occasionally gets stuck
        # in an infinite loop just before dying. I suspect it's due to one of
        # Autobus's internal threads not dying properly; I'm having autosend
        # therefore listen for SIGUSR1 on Linux and dump all thread traces when it
        # happens. Hopefully I'll be able to get an idea of which thread's causing
        # the issues that way.
        if signal:
            signal.signal(signal.SIGUSR1, dump_thread_traces)
        args = parser.parse_args()
        if args.mode is None and args.type is None:
            print "Use autosend2 --help for more information."
            return
        if args.mode == "help":
            print parser.format_help().replace("!!!INFO!!!", description.strip())
            return
        
        mode = args.mode
        if mode is None:
            # Type, at least, will be present, since the "Use autosend2 --help ..."
            # message will have stopped us if it wasn't. So basically we need to
            # check to see if name is present, and if it is, then we call the
            # specified function, and if it isn't, then we search for the
            # introspection service and print a list of all supported functions.
            if args.name is not None:
                mode = "call"
            else:
                mode = "list"
        
        # Parse out the info filer from the arguments
        info_filter = parse_info_filter(args)
        
        # Now create a bus.
        with Bus() as bus:
            if mode == "discovery":
                # Print the discovery table header
                print discovery_mode_header
                # Add a listener listening for services that will print out
                # information to stdout
                bus.add_service_listener(discovery_mode_listener, info_filter=info_filter, initial=True)
                # Wait until we're interrupted
                wait_for_interrupt()
                # This empty print is to add a new line after the one with ^C on it
                # so that things look a bit prettier when all of the REMOVED
                # messages are printed.
                print 
                return
            if mode == "call":
                if not args.name:
                    print "You need to specify the name of the function to call."
                    return
                if args.multiple: # Open a multiple-service proxy, with a
                    # bind_function that will call the requested function whenever
                    # it binds to a service. The advantage of doing this instead of
                    # creating the service proxy, waiting a few seconds, then
                    # calling the function on it is that the function is called
                    # nearly immediately instead of after a delay.
                    with bus.get_service_proxy(info_filter, bind_function=
                            partial(call_mode_onbind, args), multiple=True) as proxy:
                        # Wait a few seconds before stopping the service proxy
                        time.sleep(args.time)
                else: # Open a single-service proxy, wait for it to bind to
                    # something, then call the function and pass the result to
                    # call_mode_print_result, which prints it out.
                    with bus.get_service_proxy(info_filter) as proxy:
                        proxy.wait_for_bind(timeout=args.time)
                        try:
                            call_mode_print_result(proxy[args.name](*parse_value_list(args.values)))
                        except Exception as e:
                            call_mode_print_result(e)
                return
            if mode == "list":
                # Open a service proxy
                with bus.get_service_proxy(info_filter, multiple=args.multiple) as proxy:
                    if args.multiple: # Wait a few seconds for the proxy to bind to
                        # all of the matching services
                        time.sleep(args.time)
                    else: # Wait until the proxy binds to a single matching service
                        proxy.wait_for_bind(timeout=args.time)
                    # Call the introspection function to get information about the
                    # service
                    results = proxy["autobus.get_details"]()
                    if not args.multiple:
                        # Multiple proxies return {service_id: return_value, ...}.
                        # Single proxies just return the value itself. This bit
                        # translates the single-service proxy style to the
                        # multiple-service proxy style.
                        results = {proxy.current_service_id: results}
                    if len(results) == 0:
                        print "No matching services found."
                    for i, (service_id, details) in enumerate(results.items()):
                        if i > 0:
                            print "#" * 70
                        
                        print "Service " + str(service_id) + ":"
                        if details["doc"]:
                            print "\n" + details["doc"]
                        print "=" * 70
                        functions = filter_hidden(args, details["functions"])
                        if len(functions) == 0:
                            print "No functions available on this service."
                        for j, function in enumerate(functions.values()):
                            if j > 0:
                                print "-" * 70
                            print "Function " + function["name"] + ":"
                            if function["doc"]:
                                print "\n" + function["doc"]
                        print "=" * 70
                        events = filter_hidden(args, details["events"])
                        if len(events) == 0:
                            print "No events available on this service."
                        for j, event in enumerate(events.values()):
                            if j > 0:
                                print "-" * 70
                            print "Event " + event["name"] + ":"
                            if event["doc"]:
                                print "\n" + event["doc"]
                        print "=" * 70
                        objects = filter_hidden(args, details["objects"])
                        if len(objects) == 0:
                            print "No objects available on this service."
                        for j, object in enumerate(objects.values()):
                            if j > 0:
                                print "-" * 70
                            print "Object " + object["name"] + ":"
                            if object["doc"]:
                                print "\n" + object["doc"]
                    return
            print "Unsupported mode used: " + str(mode)
    finally:
        # Same reasoning as sigusr1; dump thread traces
        time.sleep(1)
        dump_thread_traces()


def parse_info_filter(args):
    filter = {}
    filter.update(dict(args.filter_equal))
    if args.type:
        filter["type"] = args.type
    return filter


discovery_mode_header = ("Time".ljust(26) + "Event".ljust(13) + "Service ID".ljust(44)
        + "Host".ljust(15) + "Port".ljust(7) + "Info")

def discovery_mode_listener(service_id, host, port, info, event):
    text = time.ctime(time.time()).ljust(26)
    if event is autobus2.DISCOVERED:
        text += "DISCOVERED".ljust(13)
    elif event is autobus2.UNDISCOVERED:
        text += "REMOVED".ljust(13)
    else:
        text += "CHANGED".ljust(13)
    text += service_id.ljust(44)
    text += host.ljust(15)
    text += str(port).ljust(7)
    short_info = dict(info)
    del short_info["host"]
    del short_info["port"]
    del short_info["service"]
    text += str(short_info)
    print text


def call_mode_print_result(result):
    if isinstance(result, Exception):
        print type(result).__name__ + ": " + str(result)
    else:
        print result

def call_mode_onbind(args, proxy, connection, info):
    connection[args.name](*parse_value_list(args.values), safe=True,
            callback=call_mode_print_result)


def parse_value_list(values):
    return [parse_value(v) for v in values]


def parse_value(value):
    if value.startswith("="):
        return value[1:]
    if value.startswith("@"):
        return json.loads(value)
    try:
        return int(value)
    except ValueError:
        pass
    try:
        return float(value)
    except ValueError:
        pass
    return value


def filter_hidden(args, dictionary):
    new_dict = {}
    for k, v in dictionary.items():
        if args.all or not k.startswith("autobus."):
            new_dict[k] = v
    return new_dict
































