
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
    
    def update_to(self, target, new_rev):
        """
        Updates target to the revision specified by new_rev, or deletes target
        if new_rev is None.
        
        The logic for this is quite simplified at the moment as file contents
        are stored inside each revision; this is obviously quite prototypical
        and will be changed to use just diffs and history walking later on.
        When said changes are made, an old_rev parameter will be required that
        specifies the revision that target is currently at (or None if target
        doesn't yet exist).
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
                self.update_to(target.child(name), rev)
            # And that's it for folders.
        elif data["type"] == "file":
            # It's a file, so we just write its contents.
            target.write(data["contents"].decode("base64"))
        # That's pretty much it for updating right now.
    
    def commit_changes(self, parent_revs, target):
        """
        Creates a new revision with the specified parents for the specified
        target file or folder. Note that if there is only one revision in
        parent_revs and its contents are the same as target's contents, the
        revision present in parent_revs will be returned as-is.
        
        Note that switching types isn't supported right now; if target is a
        file in one of its parent revisions but is a folder now, or vice versa,
        bad things will happen.
        """
        # See if we're a file or a folder. Switching types won't be supported
        # right now, but should probably be implemented as a delete+create
        # later on.
        if old_target.is_file:
            # It's a file. Check to see if we've got either 2 or more parents,
            # zero parents, or the file's contents are different than its
            # single parent revision's contents.
            if (len(parent_revs) > 1 or
                len(parent_revs) < 1 or
                self.get_revision(parent_revs[0])["contents"] != target.read()):
                # The file's changed, or we've got more or less than just one
                # parent; create a new revision for the file and return it.
                return self.create_revision({"type": "file",
                                             "parents": parent_revs,
                                             "contents": target.read()
                                                         .encode("base64")})
        else:
            # It's a folder. First thing we do is create revisions for all of
            # our children.
            child_revs = {}
            for child in target.list():
                # We'll build up a list of parent revisions for this child by
                # scanning our own parent revisions and seeing which of them
                # have the file in question present.
                child_parents = []
                for p in parent_revs:
                    # Get this parent revision's data
                    parent_data = self.get_revision(p)
                    # Check to see if this parent revision has the child we're
                    # looking at
                    if child.name in parent_data["children"]:
                        # This child's present in the parent we're looking at;
                        # add its listed revision as a parent.
                        child_parents.append(parent_data["children"][child.name])
                # We've got a list of parents for this child; now we create a
                # revision for the child and store it in child_revs.
                child_revs[child.name] = self.commit_changes(child_parents, child)
            # We've got a dictionary of child revisions. Now we check to see if
            # we've got exactly one parent, and it has the exact same revisions
            # we do; if that's the case, we just return that parent's revision
            # as-is.
            if (len(parent_revs) == 1 and
                self.get_revision(parent_revs[0])["children"] == child_revs):
                # Same child revs, so we return the parent revision.
                return parent_revs[0]
            else:
                # Different child revs, so we create a new revision for this
                # folder and return it.
                return self.create_revision({"type": "folder",
                                             "parents": parent_revs,
                                             "children": child_revs})
            









