
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
        print "Repository created at %s." % args.repository


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
        # We don't support updating existing working directories right now.
        # TODO: This needs to be fixed.
        if working_folder.child(".filerfrom").exists:
            raise Exception("That working directory already exists. Updating "
                            "an already-existing working directory isn't "
                            "supported right now; this is a rather large issue "
                            "that will be fixed soon. For now, delete the "
                            "working directory and then try the checkout again.")
        # Make sure the revision in question actually exists, and make sure it's
        # a folder revision. If it's a file, things will mess up big time.
        if repository.get_revision(revision)["type"] != "file":
            raise Exception("That revision is a file. Only folders can be "
                            "checked out to a working copy right now. This is "
                            "mostly because I haven't decided how individual "
                            "file revisions should be properly checked out; if "
                            "you have any suggestions, let me know "
                            "(alex@opengroove.org).")
        # Create the working folder if it doesn't exist
        working_folder.mkdirs(silent=True)
        # If it's the same dir as the repository folder, use "." as the path to
        # make it possible to just pick up the repository, contents and all, and
        # drop it somewhere else, which wouldn't be possible if we used an
        # absolute path here
        if repository_folder == working_folder:
            working_folder.child(".filerfrom").write(".")
        # If it's a different folder, use an absolute path to the repository.
        # TODO: Add a command-line switch to choose whether this is an absolute
        # path or whether this is a relative path. And perhaps consider adding
        # a switch that instructs the whole thing to not even store a path to
        # the repository; the --repository flag would then have to be given to
        # every command run on this working folder that needs access to the
        # repository. (There could be potential use cases for this, but as I
        # have yet to think of any, I won't be implementing it right now.)
        else:
            working_folder.child(".filerfrom").write(repository_folder.native_path)
        # See if we're supposed to checkout a revision. Since existing working
        # folders can't be updated to new revisions, the only real point to
        # this is to 
        if revision:
            working_folder.child(".filercurrent").write(revision)
            repository.update_to(working_folder, revision)


@command("commit")
class Commit(Command):
    pass


@command("log")
class Log(Command):
    pass

# Delete the command decorator since we don't need it anymore
del command







