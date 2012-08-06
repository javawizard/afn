
from filer1.commands.command import Command
from filer1.repository import (Repository, init_repository,
                               detect_working, detect_repository)
from filer1 import bec, exceptions
from afn.utils.partial import Partial
from afn.fileutils import File
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
        parser.add_argument("-d", "--repository", required=True)
    
    def run(self, args):
        init_repository(File(args.repository))
        print "Repository created at %s." % args.repository


@command("checkout")
class Checkout(Command):
    def update_parser(self, parser):
        parser.add_argument("-d", "--repository", required=False)
        parser.add_argument("-w", "--working", required=False)
        parser.add_argument("-r", "--revision", default=None)
    
    def run(self, args):
        if not args.repository: # --repository not specified; try to detect it
            repository_folder = detect_repository(File())
            if not repository_folder:
                raise Exception("You're not inside a repository (or an already "
                                "existing working folder) right now, and you "
                                "didn't specify --repository.")
        else:
            repository_folder = File(args.repository)
        repository = Repository(repository_folder)
        if args.working:
            working_folder = File(args.working)
        else:
            print "Using the repository directory as the working directory"
            working_folder = repository_folder
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
        # Obviously we don't check anything if we weren't requested to actually
        # check out a revision.
        if revision is not None and repository.get_revision(revision)["type"] != "folder":
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
            working_folder.child(".filerfrom").write(repository_folder.path)
        # See if we're supposed to checkout a revision. Since existing working
        # folders can't be updated to new revisions, the only real point to
        # this is to create new history lines in the repository by creating a
        # blank working copy.
        if revision:
            # Yep, we're supposed to check out a revision. Check out the
            # revision and store its revstate.
            working_folder.child(".filerstate").write(bec.dumps(
                    repository.update_to(working_folder, revision)))
        else:
            # Nope, no revision to check out to. Write an empty revstate to
            # .filerstate
            working_folder.child(".filerstate").write(bec.dumps(
                    {"parents": [], "children": {}}))

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
        print json.dumps(bec.load(sys.stdin), indent=4)    


# Delete the command decorator since we don't need it anymore
del command







