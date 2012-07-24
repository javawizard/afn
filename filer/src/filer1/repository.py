
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
        # Likewise, but for 
    
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
        object (not a string). A (hash, number) tuple will be returned.
        """
        data = json.dumps(data, sort_keys=True)
        hash = hashlib.sha1(data).hexdigest()
        self.revisions.child(hash).write(data)
        # TODO: Implement a better algorithm for searching for the next number;
        # perhaps start at 0 and double until an unused number N is hit, then
        # do a binary search from 0 to N for the lowest unused number
        number = 1
        while self.numbers.child(str(number)).exists:
            number += 1
        self.numbers.child(str(number)).write(hash)
        if data["parent"]:
            c.execute("insert into parents (parent, child) values (?, ?)",
                    data["parent"], data)
        return hash, c.lastrowid
    
    def update_to(self, revision, target, source):
        """
        Updates target, which should currently be at the revision named by
        source (which can be null if target does not currently exist), to the
        specified revision. source must be an ancestor of target; going
        backward in history is not currently supported. source can, of course,
        be None.
        """
        # This is a bit of a complicated process. What we have to do is
        # repeatedly query the parents table to get the target revision's
        # parents until we hit the source revision, or if the source revision's
        # None, then query the parent table until we hit a revision that doesn't
        # have a parent. Then we use update_single_to to update to all the
        # intermediate revisions along the way.
        chain = []
        current = revision
        while current != source:
            with self.db.cursor as c:
                # Add the current revision to the list of revisions to jump to
                chain.append(current)
                # Look up this revision's parent
                c.execute("select parent from parents where child = ?", current)
                current = c.fetchone()
                # current will now be either None if there's no parent or a
                # (parent,) tuple; unwrap it if it's the latter
                if current:
                    current, = current
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
        data = self.get_revision(revision)
        # Get the parent revision's data, if the parent exists
        parent_data = None
        if data["parent"]:
            parent_data = self.get_revision(data["parent"])
        if data["type"] == "folder":
            # It's a folder, so create a folder for it, if one doesn't exist
            target.mkdir(silent=True)
            # Iterate through the children that changed this revision
            for name, name_rev in data["children"].items():
                if name_rev and not (parent_data or {})["children"].get(name):
                    # File or folder was added; check out the relevant revision
                    self.update_to(name_rev, target.child(name), None)
                elif name_rev:
                    # File or folder was updated to a different revision; update it
                    self.update_to(name_rev, target.child(name), parent_data["children"][name])
                else:
                    # File or folder was deleted; delete it
                    to_delete = target.child(name)
                    if to_delete.is_folder:
                        to_delete.delete_folder(True)
                    else:
                        to_delete.delete()
        elif data["type"] == "file":
            # It's a file; we'll either have contents or diff here; contents
            # means overwrite (or create) with given contents, diff means patch
            # the file with the given diff
            if "contents" in data:
                # Write new contents to file. Usually used when creating new
                # files, but can be used when including the entire contents is
                # more space efficient than including a diff
                target.write(data["contents"])
            elif "diff" in data:
                # Read the file
                contents = target.read()
                # Update the file
                contents = bsdiff4.patch(contents, data["diff"])
                # Write the file
                target.write(contents)
            else:
                raise exceptions.MissingFileInfo(data=data)
        else:
            raise exceptions.InvalidType(type=data["type"])
            









