
from filer1.commands.command import Command
from afn.utils.partial import Partial

commands = {}
# Convoluted bit of magic: after this line, command(x)(y) is the same as
# commands[x] = y. This allows decorating all of the command classes with
# @command("command-name") to automatically add them to the commands dict.
# The downside is that the individual command names get set to None (as that's
# what dict.__setitem__ returns), but I can't think of an instance where they'd
# be actually needed separately, so that's fine for now.
command = Partial(Partial, commands.__setitem__)

# And now for the actual commands.

@command("checkout")
class Checkout(Command):
    pass


@command("commit")
class Commit(Command):
    pass


@command("log")
class Log(Command):
    pass

# Delete the command decorator since we don't need it anymore
del command







