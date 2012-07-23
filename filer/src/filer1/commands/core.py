
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
        repository_folder = File(args.repository)
        repository = Repository(repository_folder)
        working_folder = File(args.working)
        revision = args.revision
        if working_folder.child(".filerfrom").exists:
            raise Exception("That working directory already exists.")
        working_folder.mkdirs()
        if repository_folder == working_folder:
            working_folder.child(".filerfrom").write(".")
        else:
            working_folder.child(".filerfrom").write(repository_folder.native_path)
        if revision:
            working_folder.child(".filercurrent").write(revision)
        repository.export(revision, working_folder)


@command("commit")
class Commit(Command):
    pass


@command("log")
class Log(Command):
    pass

# Delete the command decorator since we don't need it anymore
del command







