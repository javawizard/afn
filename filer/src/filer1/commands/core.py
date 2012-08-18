
from filer1.commands.command import Command
from filer1.repository import (Repository, init_repository,
                               detect_working, detect_repository)
from filer1.working import WorkingCopy
from filer1 import bec, exceptions
from afn.utils.partial import Partial
from afn.fileutils import File, file_or_none
import time
import sys
import json

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
        parser.add_argument("location")
        parser.add_argument("--plain", default=False, action="store_true",
                help="Normally, a repository is created in the .filer folder "
                "at the specified location, and the location is initialized "
                "to be a working copy. This option causes the location itself "
                "to be used as the repository.")
    
    def run(self, args):
        location = File(args.location)
        if args.plain:
            repository_folder = location
        else:
            location.mkdirs()
            repository_folder = location.child(".filer")
        init_repository(repository_folder)
        if not args.plain:
            repository = Repository(repository_folder)
            working = WorkingCopy(repository, location)
            working.create()
        print "Repository created at %r." % repository_folder.path


@command("checkout")
class Checkout(Command):
    def update_parser(self, parser):
        parser.add_argument("-d", "--repository", required=False)
        parser.add_argument("-w", "--working", required=False)
        parser.add_argument("-r", "--revision", required=True)
    
    def run(self, args):
        if args.repository:
            repository_folder = File(args.repository)
        else:
            repository_folder = detect_repository()
        repository = Repository(repository_folder)
        if args.working:
            working_folder = File(args.working)
        else:
            print "Using the repository directory as the working directory"
            working_folder = repository_folder
        revision = args.revision
        working = WorkingCopy(repository, working_folder)
        # We don't support updating existing working directories right now.
        # TODO: This needs to be fixed.
        if working.is_working():
            print "That's already a working copy. Are you sure you want to "
            print "overwrite its contents? (y or n)"
            if raw_input().lower()[0] != "y":
                print "Aborting."
                return
        data = repository.get_revision(revision)
        if not working_folder.exists:
            if data["type"] == "folder":
                working_folder.mkdirs(True)
            else:
                working_folder.write("")
        if not working.is_working():
            working.create()
        # Check out the requested revision
        working.update_to(revision)

@command("commit")
class Commit(Command):
    def update_parser(self, parser):
        parser.add_argument("-w", "--working", default=None)
        parser.add_argument("-m", "--message", required=True)

    def run(self, args):
        # This is basically as simple as asking the repository to commit a new
        # revision on the current working directory (which we figure out by
        # jumping parents until we find .filerfrom) and then updating
        # .filerstate to contain the new revstate.
        if args.working is None:
            working_folder = detect_working(File("."))
        else:
            working_folder = File(args.working)
        if working_folder is None:
            raise Exception("You're not inside a working folder right now, "
                            "and you didn't specify --working.")
        repository_folder = File(working_folder.child(".filerfrom").read())
        repository = Repository(repository_folder)
        # We've got the repository. Now we go read the revstate to use.
        revstate = bec.loads(working_folder.child(".filerstate").read())
        # Then we write down the date and commit message
        info = {"date": time.time(), "message": args.message}
        # Then we create the new revision
        new_revstate = repository.commit_changes(revstate, working_folder, info)
        # Then update .filerstate to the new revstate
        working_folder.child(".filerstate").write(bec.dumps(new_revstate))
        # And last of all, we print out a message about the commit.
        hash = new_revstate["parents"][0]
        print "Committed revision %s:%s." % (repository.number_for_rev(hash), hash)


@command("log")
class Log(Command):
    def update_parser(self, parser):
        parser.add_argument("-d", "--repository", required=False)
    
    def run(self, args):
        if args.repository:
            repository_folder = File(args.repository)
        else:
            repository_folder = detect_repository(File())
            if not repository_folder:
                raise Exception("You're not in a repository (or a working "
                                "folder) right now and you didn't specify "
                                "--repository.")
        repository = Repository(repository_folder)
        print
        for number, hash, data in repository.revision_iterator():
            print "Revision %s:%s:" % (number, hash)
            print "    date:           %s" % time.ctime(data.get("info", {}).get("date", 0))
            print "    type:           %s" % data["type"]
            if data.get("current_name"):
                print "    committed as:   %s" % data["current_name"]
            for cparent in data["parents"]:
                print "    change parent:  %s:%s" % (repository.number_for_rev(cparent), cparent)
#            for dparent in repository.get_dirparents(hash):
#                print "    dir parent:     %s:%s" % (repository.number_for_rev(dparent), dparent)
            print "    message:        %s" % data.get("info", {}).get("message", "")
            print


@command("push")
class Push(Command):
    def update_parser(self, parser):
        parser.add_argument("-d", "--repository", required=False)
        parser.add_argument("target")
    
    def run(self, args):
        if args.repository:
            repository_folder = File(args.repository)
        else:
            repository_folder = detect_repository(File())
            if not repository_folder:
                raise Exception("You're not in a repository (or a working "
                                "folder) right now and you didn't specify "
                                "--repository.")
        local_repository = Repository(repository_folder)
        remote_repository = Repository(File(args.target))
        # This is a rather unintuitive way to do things, but we're just
        # going to iterate through all revisions in local_repository and check
        # to see if they're present in remote_repository, and if they're not,
        # add them.
        changes_pushed = 0
        for number, hash, data in local_repository.revision_iterator():
            if not remote_repository.has_revision(hash):
                changes_pushed += 1
                # Revision is not present, so create it
                print "Pushing revision %s:%s" % (number, hash)
                new_hash = remote_repository.create_revision(data)
                # Sanity check to make sure we didn't get a different revision
                # hash; this shouldn't actually be needed unless there's a bug
                # in the revision encoding stuff somewhere
                if new_hash != hash:
                    raise Exception("INTERNAL ERROR: Transferred hash "
                            "mismatch: %s -> %s. One of the two repositories "
                            "involved in the push is most likely corrupt."
                            % (hash, new_hash))
        if changes_pushed:
            print "Pushed %s change%s to %s." % (changes_pushed, "s" if changes_pushed > 1 else "", args.target)
        else:
            print "Remote repository is already up to date."


@command("becdump")
class BECDumpCommand(Command):
    def update_parser(self, parser):
        pass
    
    def run(self, args):
        print json.dumps(bec.loads(sys.stdin.read()), indent=4,
                default=lambda a: "<binary data>")


@command("current")
class CurrentCommand(Command):
    def update_parser(self, parser):
        parser.add_argument("-w", "--working", default=None)
    
    def run(self, args):
        if args.working is None:
            working_folder = detect_working(File("."))
        else:
            working_folder = File(args.working)
        if working_folder is None:
            raise Exception("You're not inside a working folder right now, "
                            "and you didn't specify --working.")
        revstate = bec.load(working_folder.child(".filerstate").open())
        for parent in revstate["parents"]:
            print parent

# Delete the command decorator since we don't need it anymore
del command







