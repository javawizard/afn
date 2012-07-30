
import sqlite3
from afn.fileutils import File
from contextlib import closing
import hashlib
from filer1 import exceptions
import json

def init_repository(folder):
    # TODO: Consider using a neo4j repository for the prototype. It'd make a
    # lot of stuff simpler and would do away with pretty much all of the
    # maintenance folders (numbers, numbersbyrev, changeparents,
    # changechildren, dirparents, and dirchildren).
    folder = File(folder)
    filer_dir = folder.child(".filer")
    if filer_dir.exists:
        raise Exception("There's already a filer repository at that location.")
    filer_dir.mkdirs(True)
    # We don't have any sort of transaction support right now. This is a bad
    # idea and needs to be fixed in the future.
    
    # db = sqlite3.connect(db_file.native_path, isolation_level=None)
    # c = db.cursor()
    # c.execute("create table revisions (hash text unique, data text, number integer primary key autoincrement)")
    # c.execute("create table parents (parent text, child text)")
    # db.close()


class Repository(object):
    def __init__(self, folder):
        self.folder = File(folder)
        self.filer_dir = self.folder.child(".filer")
        self.revisions = self.filer_dir.child("revisions")
        self.revisions.mkdirs(True)
        self.numbers = self.filer_dir.child("numbers")
        self.numbers.mkdirs(True)
        self.numbersbyrev = self.filer_dir.child("numbersbyrev")
        self.numbersbyrev.mkdirs(True)
        # File names are revision hashes, and their contents are JSON lists of
        # their parent revisions.
        self.changeparents = self.filer_dir.child("changeparents")
        self.changeparents.mkdirs(True)
        # File names are revision hashes, and their contents are
        # JSON lists of child revisions.
        self.changechildren = self.filer_dir.child("changechildren")
        self.changechildren.mkdirs(True)
        # File names are revision hashes, and their contents are JSON lists of
        # all commits that include this file or folder.
        self.dirparents = self.filer_dir.child("dirparents")
        self.dirparents.mkdirs(True)
        # File names are revision hashes, and their contents are JSON lists of
        # all commits that the revision in question includes. For files, this
        # will be an empty list, since files don't include anything; for
        # folders, this will be the list of revisions of the folder's children.
        self.dirchildren = self.filer_dir.child("dirchildren")
        self.dirchildren.mkdirs(True)
    
    def get_revision(self, id):
        """
        Gets the revision with the specified revision id, which can either be a
        hex string representing the full sha1 hash or the revision's numeric id.
        A ValueError will be thrown if no such revision exists.
        
        The return value is a JSON object corresponding to the revision.
        
        Note that short revision numbers must still be given as strings. Bad
        things (exceptions mainly) will happen if ints are passed in instead.
        """
        if self.revisions.child(id).exists: # Revision with the same hash exists
            return json.loads(self.revisions.child(id).read())
        if self.numbers.child(id).exists: # Revision with that number exists
            return json.loads(self.revisions.child(self.numbers.child(id).read()).read())
        raise Exception("The revision %r does not exist." % id)
    
    def create_revision(self, data):
        """
        Creates a new revision with the specified data, which should be a JSON
        object (not a string). The new revision's hash will be returned.
        
        Entries in changeparents, changechildren, dirparents, and dirchildren
        will be created for this new revision.
        """
        text_data = json.dumps(data, sort_keys=True)
        hash = hashlib.sha1(text_data).hexdigest()
        self.revisions.child(hash).write(text_data)
        # TODO: Implement a better algorithm for searching for the next number;
        # perhaps start at 0 and double until an unused number N is hit, then
        # do a binary search from 0 to N for the lowest unused number
        number = 1
        while self.numbers.child(str(number)).exists:
            number += 1
        # Add an entry into numbers for this number
        self.numbers.child(str(number)).write(hash)
        # Add a reverse entry into numbersbyrev
        self.numbersbyrev.child(hash).write(str(number))
        # Write our list of parents to a changeparents file created for us
        self.changeparents.child(hash).write(json.dumps(data["parents"])
        # Write an empty changechildren file for ourselves
        self.changechildren.child(hash).write(json.dumps([]))
        # Iterate over our parents and write ourselves into their respective
        # changechildren files
        for p in data["parents"]:
            f = self.changechildren.child(p)
            f.write(json.dumps(json.loads(f.read()) + [hash]))
        # TODO: write stuff to dirparents and dirchildren
        return hash
    
    def update_to(self, target, old_rev, new_rev):
        """
        Updates target, which should currently be at the revision named by
        old_rev (which can be null if target does not currently exist), to the
        revision specified by new_rev.
        
        The logic for this is quite simplified at the moment as file contents
        are stored inside each revision; this is obviously quite prototypical
        and will be changed to use just diffs and history walking later on.
        
        Actually, because history walking isn't used right now, old_rev is
        entirely ignored, so it can be anything right now.
        
        new_rev can, of course, be None to just delete the specified target
        instead.
        """
        # If new_rev is None, delete target.
        if new_rev is None:
            if target.is_folder:
                target.delete_folder(True)
            else:
                target.delete()
            return
        # new_rev isn't None, so we need to update to it. First we need to get
        # the relevant revision's data.
        data = self.get_revision(new_rev)
        # Then we delete the target so that we can start off with a clean slate.
        # Obviously we need to do something a bit better once we get past the
        # prototype stage. TODO: Modify fileutils to just use one function for
        # deleting things; I've written stuff down on why I kept things as two
        # separate methods, but I've decided I want them together, as it'll get
        # rid of a bunch of if/else statements like I've got here.
        if target.is_folder:
            target.delete_folder(True)
        else:
            target.delete()
        # Now we check to see if we're dealing with a file or a folder.
        if data["type"] == "folder":
            # It's a folder, so we need to create a new folder for it.
            target.mkdir()
            # Now we go iterate through the folder's children and update each
            # of them.
            for name, rev in data["children"]:
                # As mentioned in the 
                self.update_to(
    
    def commit_changes(self, old_revspec, old_target, new_target):
        """
        Creates a new revision by comparing old_target, which should be at the
        revspec old_spec, with new_target. The new revspec will be returned.
        """
        # See if we're a file or a folder. Switching types won't be supported
        # right now, but should probably be implemented as a delete+create
        # later on.
        if old_target.is_file:
            # It's a file. Check to see if the new contents are different from
            # the old contents.
            old_contents = old_target.read() if old_target.exists else None
            new_contents = new_target.read() # File removal shouldn't ever get
            # to this level, as files are deleted by removing them from their
            # parent directory, not by marking the file itself as deleted
            if old_contents == new_contents:
                # The file hasn't changed, so return the current revspec.
                return old_revspec
            else:
                # The file's changed; write a new revision for the file, and
                # return the new revspec.
                return {"rev": self.create_revision({"type": "file",
                                                     "old": old_contents,
                                                     "new": new_contents)}
        else:
            # It's a folder. TODO: How do we go about doing this? We need to
            # keep track somewhere which revisions each child file is currently
            # at so that we can pass them into commit_changes as old_rev, and
            # so that we can see if the file's changed revisions as reported by
            # commit_changes. How this is stored needs to be thought out a bit
            # more; perhaps store a JSON dict of folder/file current revisions
            # as a file inside the working directory, and then have
            # commit_changes return it or something instead of the hash. Needs
            # a bit more thought.
            # We could always have old_rev (and the return value from
            # commit_changes) be a {"rev": ..., "children": {"child1":
            # {"rev": ..., ...}, ...}} dict. The children key would be null for
            # files, and would map child names to dicts of the same format for
            # folders. That'd let us know what the old revision is, and we can
            # return new versions of the dict whenever we have to commit a file
            # or a folder because of changes. Then we just commit all of the
            # files/folders inside a file/folder, and if any of the dicts we
            # get out are different, we add a change, with the old value being
            # the old dict's rev key and the new value being the new dict's rev
            # key, or null for either of those that didn't exist.
            # TODO: Still need to think about how to handle deleted
            # files/folders properly.
            raise NotImplementedError
            









