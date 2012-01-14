
try:
    from argparse import ArgumentParser
except ImportError:
    from afn.backports.argparse import ArgumentParser
from afn.utils.argparseutils import AppendWithConst
from afn.utils.partial import partial

parser = ArgumentParser()

parser.add_argument("-j", "--json", )

actions = parser.add_argument_group("actions")
add_action = partial(actions.add_argument, dest="actions", action=AppendWithConst)
add_action("-u", const="up", nargs=0)
add_action("-d", const="down", nargs=0)

def main():
    args = parser.parse_args()
    print args
