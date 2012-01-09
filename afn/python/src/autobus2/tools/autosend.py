
try:
    import argparse
except ImportError:
    from afn.backports import argparse
from functools import partial
from autobus2 import Bus, wait_for_interrupt

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
options.add_argument("-m", "--filter-equal", action="append", dest="filter_equal",
        nargs=2, metavar=("key", "value"),
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
            bus.add_service_listener()


def parse_info_filter(args):
    filter = {}
    filter.update(dict(args.filter_equal))
    return filter
    
































