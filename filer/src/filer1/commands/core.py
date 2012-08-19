
from filer1.commands.command import Command
from filer1.repository import (Repository, init_repository,
                               detect_working, detect_repository)
from filer1.working import WorkingCopy
from filer1 import bec, exceptions
from filer1.constants import XATTR_BASE, XATTR_REPO
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
        parser.add_argument("-r", "--revision", required=False)
    
    def run(self, args):
        # The checkout command does something different depending on how it's
        # run:
        # 
        # If the working copy (specified with --working or auto-detected)
        # exists and -r is specified, the working copy is updated to the
        # revision in question.
        # If the working copy exists but is not a working copy, it is turned
        # into one. Then, if -r is specified, it is updated to the specified
        # revision.
        # If the working copy does not exist, -r must be specified (so that we
        # know whether to create a file or a folder), and the working copy
        # will be created and updated to the revision in question.
        # 
        # So, if the working copy does not exist, we require -r and create it.
        # Then, if it's not a working copy, we make it one by setting
        # XATTR_REPO. Then we go update the working copy to the specified
        # revision.
        if args.repository:
            repository_folder = File(args.repository)
        else:
            repository_folder = detect_repository()
        repository = Repository(repository_folder)
        if args.working:
            working_file = File(args.working)
        else:
            working_file = detect_working()
        revision = args.revision
        # Check to see if the soon-to-be working copy already exists as a file
        # or folder
        if not working_file.exists:
            # It doesn't exist. Make sure we've got a --revision (and error out
            # if we don't, since then we don't know whether to create a file or
            # a folder).
            if revision is None:
                raise Exception("When using the checkout command on a working "
                        "copy that doesn't actually exist yet, --revision must "
                        "be specified. This is because Filer doesn't know "
                        "whether to create a folder or a file for it. If you "
                        "want to create a new, blank working copy without "
                        "checking one out from a revision, create a file or "
                        "folder at the relevant location, then use the "
                        "checkout command again. Then it'll work.")
            # We've got a revision, so look at whether it's a file or a folder,
            # and create a file or a folder accordingly.
            working_type = repository.get_revision(revision)["type"]
            if working_type == "file":
                # It's a file, so create a blank file for it.
                working_file.write("")
            else:
                # It's a folder, so create a blank folder for it.
                working_file.mkdir()
        # The working file exists. Now see if it's a working copy.
        working = WorkingCopy(repository, working_file)
        if not working.is_working():
            # It's not a working copy, so make it a working copy.
            working.create()
        # Now update it. TODO: We might want to keep track of whether it
        # already existed before now, and if it did, warn the user that they'll
        # be overwriting their changes.
        if revision:
            working.update_to(revision)
            print "Checked out revision %r" % revision
        else:
            print "Not checking out a new revision."


@command("add")
class Add(Command):
    def update_parser(self, parser):
        parser.add_argument("file")
    
    def run(self, args):
        file = File(args.file)
        if file.has_xattr(XATTR_BASE):
            print "That file has already been added."
            return
        file.set_xattr(XATTR_BASE, json.dumps([]))
        print "%r is now being tracked. You should `filer commit` soon." % file.path


@command("untrack")
class Untrack(Command):
    def update_parser(self, parser):
        parser.add_argument("file")
    
    def run(self, args):
        file = File(args.file)
        if not file.has_xattr(XATTR_BASE):
            print "That file isn't being tracked."
            return
        file.delete_xattr(XATTR_BASE)
        print "%r has been untracked. You should `filer commit` soon." % file.path


@command("commit")
class Commit(Command):
    def update_parser(self, parser):
        parser.add_argument("-d", "--repository", default=None)
        parser.add_argument("-w", "--working", default=None)
        parser.add_argument("-m", "--message", required=True)

    def run(self, args):
        # This is basically as simple as getting the working file and the
        # repository and committing a new revision on them.
        if args.repository:
            repository_folder = File(args.repository)
        else:
            repository_folder = detect_repository()
        repository = Repository(repository_folder)
        if args.working:
            working_file = File(args.working)
        else:
            working_file = detect_working()
        info = {"date": time.time(), "message": args.message}
        working = WorkingCopy(repository, working_file)
        # Keep track of the old base (which will be None if the working copy
        # is untracked) and compare it later on to see if anything changed
        base = json.loads(working_file.get_xattr(XATTR_BASE))
        working.commit(info)
        if not working_file.has_xattr(XATTR_BASE):
            # This can happen when the working file itself isn't tracked, which
            # is rare but happens if the user checks out a new, blank working
            # copy but doesn't add it with the add command. In that case, we
            # print a friendly warning.
            print "The working copy isn't being tracked. You probably need to "
            print "`filer add %s` first." % working_file.path
        else:
            new_base = json.loads(working_file.check_xattr(XATTR_BASE))
            hash = new_base[0]
            if new_base != base:
                print "Committed revision %s:%s." % (repository.number_for_rev(hash), hash)
            else:
                print "No changes to be committed."
                print "If you copied new files into the working copy that you "
                print "expected to show up, make sure to `filer add` them first."


@command("log")
class Log(Command):
    def update_parser(self, parser):
        parser.add_argument("-d", "--repository", required=False)
        parser.add_argument("-a", "--all", default=False, action="store_true")
    
    def run(self, args):
        if args.repository:
            repository_folder = File(args.repository)
        else:
            repository_folder = detect_repository(File())
            if not repository_folder:
                raise Exception("You're not in a repository (or a working "
                                "folder) right now and you didn't specify "
                                "--repository.")
        if args.working:
            working_file = File(args.working)
        else:
            working_file = detect_working(silent=True)
        repository = Repository(repository_folder)
        revisions = None
        if working_file and working_file.has_xattr(XATTR_BASE) and not args.all:
            # We have a working file and we're not displaying all revisions.
            # Only show revisions which are ancestors of the working file.
            # TODO: Figure out a way to do this without having to reconstruct
            # every revision; probably need to have some sort of cache in the
            # Repository class of parent-child revision relationships. Still
            # thinking a Neo4j database might be useful for this, or maybe some
            # sort of equivalent SQLite database.
            # TODO: What should we do if multiple parents are present? Ideally
            # we'd interleave their history and show things in numerical order,
            # but right now we're just using the first parent.
            revisions = set()
            base = json.loads(working_file.get_xattr(XATTR_BASE))
            if len(base) > 0:
                current = base[0]
                if len(base) > 1:
                    print "Warning: multiple parents are present in the "
                    print "working copy. Only the first will be shown. This "
                    print "will be changed soon to show all of the parents."
                
        print
        for number, hash, data in repository.revision_iterator():
            if revisions is not None and hash not in revisions:
                continue
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







