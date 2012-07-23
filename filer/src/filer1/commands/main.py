
import sys
import os
from afn.backports.argparse import ArgumentParser
from filer1.commands.core import commands


def main():
    if len(sys.argv) <= 1:
        print "Usage: filer <command>"
        print "More usage information will be coming soon."
        print "Or you can read through filer's source; see"
        print "http://hg.opengroove.org/afn/file/default/filer to read it."
    command_name = sys.argv[1]
    try:
        command = commands[command_name]
    except KeyError:
        print "The command %r does not exist." % command
        print "Valid commands are %r" % commands.keys()
        sys.exit(1)
    parser = ArgumentParser()
    command.update_parser(parser)
    args = parser.parse_args(sys.argv[2:])
    command.run(args)


if __name__ == "__main__":
    main()
