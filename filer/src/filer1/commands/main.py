
import sys
import os
from afn.backports.argparse import ArgumentParser


def main():
    if len(sys.argv) <= 1:
        print "Usage: filer <command>"
        print "More usage information will be coming soon."
        print "Or you can read through filer's source; see"
        print "http://hg.opengroove.org/afn/file/default/filer to read it."
    parser = ArgumentParser()
    


if __name__ == "__main__":
    main()
