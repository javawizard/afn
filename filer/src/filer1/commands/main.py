
import sys
import os
try:
    import afn
    del afn
except ImportError:
    sys.path.append(os.path.join(os.path.dirname(os.path.dirname(__file__)), "afn", "python", "src"))
    import afn
    del afn
from afn.backports.argparse import ArgumentParser


def main():
    parser = ArgumentParser()


if __name__ == "__main__":
    main()
