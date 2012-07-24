
import sqlite3
from afn.fileutils import File
from contextlib import closing
import hashlib
from filer1 import exceptions
import json

def init_repository(folder):
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
        self.numbersbyrev.mkdirs(True)s
        # File names are revision hashes, and their contents are their parent
        # revision hash. This is only one revision right now; in the future,
        # when merges are supported, this will be a newline-separated list of
        # parent revisions.
        self.changeparents = self.filer_dir.child("changeparents")
        self.changeparents.mkdirs(True)
        # File names are revision hashes, and their contents are
        # newline-separated lists of child revisions
        self.changechildren = self.filer_dir.child("changechildren")
        self.changechildren.mkdirs(True)
        # Likewise, but for dirlines instead of changelines
        self.dirparents = self.filer_dir.child("dirparents")
        self.dirparents.mkdirs(True)
        self.dirchildren = self.filer_dir.child("dirchildren")
        self.dirchildren.mkdirs(True)
    
    def get_revision(self, id):
        """
        Gets the revision with the specified revision id, which can either be a
        hex string representing the full sha1 hash or the revision's numeric id.
        A ValueError will be thrown if no such revision exists.
        
        The return value is a JSON object corresponding to the revision.
        
        Note that short revision numbers must still be given as strings.
        """
        if self.revisions.child(id).exists:
            return json.loads(self.revisions.child(id).read())
        if self.numbers.child(id).exists:
            return json.loads(self.revisions.child(self.numbers.child(id).read()).read())
    
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
        self.numbers.child(str(number)).write(hash)
        self.numbersbyrev.child(hash).write(str(number))
        # If we've got a parent, write a changeparents file for our revision
        # indicating our parent and write a changechildren file for the parent
        # indicating we're one of its children. This will need to be changed
        # when commits are allowed to have multiple parents.
        if data["parent"]:
            self.changeparents.child(hash).write(self["parent"])
            self.changechildren.child(self["parent"]).append(hash + "\n")
        # TODO: write stuff to dirparents and dirchildren
        return hash
    
    def update_to(self, target, old_rev, new_rev):
        """
        Updates target, which should currently be at the revision named by
        old_rev (which can be null if target does not currently exist), to the
        specified revision. old_rev must be an ancestor of new_rev; going
        backward in history is not currently supported. old_rev can, of course,
        be None, and new_rev can be None to delete the relevant file/folder
        instead.
        """
        # This is a bit of a complicated process. What we have to do is
        # repeatedly check the changeparents dir to walk back from new_rev
        # until we hit old_rev, or if old_rev is None, until we hit a revision
        # with no parent. Then we use update_single_to to apply all of the
        # changes. The exception is, of course, if new_rev is None, in which
        # case we just delete target.
        if new_rev is None:
            if target.is_folder:
                target.delete_folder(True)
            else:
                target.delete()
            return
        # The chain of revisions to apply
        chain = []
        # The current revision we're looking at
        current = revision
        while current != source:
            if current is None: # We went too far: we hit a revision with no
            # parents but we didn't find the one we were looking for
                raise Exception(target, old_rev, new_rev, chain)
            # Everything seems to have gone ok, so we can proceed.
            # Add the current revision to the list of revisions to jump to
            chain.append(current)
            # Look up this revision's parent
            parent_file = self.changeparents.child(current)
            # If it exists, mark it as the new current revision.
            if parent_file.exists:
                current = parent_file.read()
            # If it doesn't exist, we've hit the end of the line, so we write
            # down None.
            else:
                current = None
        # We now have the chain of revisions to follow, so follow it.
        for r in chain:
            self.update_single_to(r, target)
        # And that's it!
        # TODO: Add support to this method for updating to any revision,
        # regardless of whether it's an ancestor or a descendent or a cousin.
        # Updating to revisions unrelated to this one probably won't be
        # supported. (I define related to mean that the revisions are either
        # ancestor/descendent or cousins, cousins meaning that they share a
        # common ancestor.)
    
    def update_single_to(self, revision, target):
        """
        Updates target, which should currently be checked out to the specified
        revision's parent (or should not exist, if the specified revision has
        no parent), to the specified revision.
        """
        # Get the revision's data
        data = self.get_revision(revision)
        # Get the parent revision's data, if the parent exists
        parent_data = None
        if data["parent"]:
            parent_data = self.get_revision(data["parent"])
        if data["type"] == "folder":
            # It's a folder, so create a folder for it, if one doesn't exist
            target.mkdir(silent=True)
            # Iterate through the children that changed this revision and
            # update each of them. update_to will take care of adding new
            # children and deleting ones that were removed.
            for name, (old_rev, new_rev) in data["children"].items():
                    self.update_to(target.child(name), old_rev, new_rev)
        elif data["type"] == "file":
            # It's a file. Right now we'll have two entires in data, old and
            # new, which are base64-encoded versions of the file; old will be
            # None if the file didn't exist. In the future, we'll have some sort
            # of diff stored. I just need to find a binary diff algorithm that
            # supports three-way diffs and is reversible first.
            target.write(data["new"])
        else:
            raise exceptions.InvalidType(type=data["type"])
    
    def commit_changes(self, old_rev, old_target, new_target):
        """
        Creates a new revision by comparing old_target, which should be at the
        revision old_rev, with new_target. The hash will be returned.
        """
        # Get the old revision's data
        old_rev_data = self.get_revision(old_rev)
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
                # The file hasn't changed, so return the current revision.
                return old_rev
            else:
                # The file's changed; write a new revision for the file, and
                # return the new revision's hash.
                return self.create_revision({"type": "file",
                                             "old": old_contents,
                                             "new": new_contents)
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
            raise NotImplementedError
            









