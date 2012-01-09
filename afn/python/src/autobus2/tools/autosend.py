
try:
    import argparse
except ImportError:
    from afn.backports import argparse
from functools import partial
from autobus2 import Bus, wait_for_interrupt
import autobus2
import time

description = """
A command-line Autobus client.
"""

parser = argparse.ArgumentParser(prog="autosend2", description="!!!INFO!!!", add_help=False)

modes = parser.add_argument_group("modes").add_mutually_exclusive_group()
add_mode = partial(modes.add_argument, action="store_const", dest="mode")
add_mode("-f", "--call", const="call",
        help="Function call mode")
add_mode("-b", "--broadcast", const="broadcast",
        help="Broadcast watch mode")
add_mode("-d", "--discovery", const="discovery",
        help="Discovery mode")
add_mode("-?", "--help", const="help",
        help="Show this help message")

options = parser.add_argument_group("optional arguments")
options.add_argument("-q", "--filter-equal", action="append", dest="filter_equal",
        nargs=2, metavar=("key", "value"), default=[],
        help="The specified key must have the specified value.")

def main():
    args = parser.parse_args()
    if not args.mode:
        print "Use autosend2 --help for more information."
        return
    if args.mode == "help":
        print parser.format_help().replace("!!!INFO!!!", description.strip())
        return
    
    info_filter = parse_info_filter(args)
    
    with Bus() as bus:
        if args.mode == "discovery":
            print discovery_mode_header
            bus.add_service_listener(discovery_mode_listener, info_filter=info_filter, initial=True)
            wait_for_interrupt()
            print 
            return


def parse_info_filter(args):
    filter = {}
    filter.update(dict(args.filter_equal))
    return filter


discovery_mode_header = ("Time".ljust(26) + "Event".ljust(13) + "Service ID".ljust(42)
        + "Host".ljust(15) + "Port".ljust(7) + "Info")

def discovery_mode_listener(service_id, host, port, info, event):
    text = time.ctime(time.time()).ljust(26)
    if event is autobus2.DISCOVERED:
        text += "DISCOVERED".ljust(13)
    elif event is autobus2.UNDISCOVERED:
        text += "REMOVED".ljust(13)
    else:
        text += "CHANGED".ljust(13)
    text += service_id.ljust(42)
    text += host.ljust(15)
    text += str(port).ljust(7)
    short_info = dict(info)
    del short_info["host"]
    del short_info["port"]
    del short_info["service"]
    text += str(short_info)
    print text
































