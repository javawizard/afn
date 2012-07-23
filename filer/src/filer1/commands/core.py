
from filer1.commands.command import Command
from filer1.repository import Repository, init_repository
from afn.utils.partial import Partial
from afn.fileutils import File

commands = {}
# Convoluted bit of magic: after this line, command(x)(y) is the same as
# commands[x] = y. This allows decorating all of the command classes with
# @command("command-name") to automatically add them to the commands dict.
# The downside is that the individual command names get set to None (as that's
# what dict.__setitem__ returns), but I can't think of an instance where they'd
# be actually needed separately, so that's fine for now.
command = Partial(Partial, commands.__setitem__)

# And now for the actual commands.

@command("init")
class Init(Command):
    def update_parser(self, parser):
        parser.add_argument("-d", "--repository")
    
    def run(self, args):
        init_repository(File(args.repository))


@command("checkout")
class Checkout(Command):
    def update_parser(self, parser):
        parser.add_argument("-d", "--repository")
        parser.add_argument("-w", "--working")
        parser.add_argument("-r", "--revision", default=None)
    
    def run(self, args):
        repository = Repository(File(args.repository))
        working = File(args.working)
        revision = args.revision
        if working.child(".filerfrom").exists:
            raise Exception("That working directory already exists.")
        working


@command("commit")
class Commit(Command):
    pass


@command("log")
class Log(Command):
    pass

# Delete the command decorator since we don't need it anymore
del command







