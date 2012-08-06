
import sqlite3
from afn.fileutils import File
from contextlib import closing
import hashlib
from filer1 import exceptions
from filer1 import bec

global_debug = False


def delete(target):
    """
    Deletes the specified file or folder. This mainly exists due to a limitation
    of fileutils, namely that two separate methods are used to delete folders
    and files; I intend to change fileutils to have the same method be used for
    both, at which point this function will become obsolete.
    """
    if target.is_folder:
        target.delete_folder(True)
    else:
        target.delete()


def detect_working(target):
    """
    Finds the current working directory by jumping parents until we arrive at
    the parent that contains a .filerfrom file. The folder containing said file
    is then returned. If no such folder is found before we hit the directory
    root, None is returned.
    """
    while not target.child(".filerfrom").exists:
        target = target.parent
        if target is None:
            return None
    return target


def detect_repository(target):
    """
    Finds the current repository by jumping parents until we arrive at the
    parent that contains a .filer directory. The folder containing said 
    directory is then returned. If no such folder is found before we hit the
    directory root, detect_working is used to see if we're in a working folder,
    and if we are, its .filerfrom is used. Otherwise, None is returned.
    """
    current = target
    while not current.child(".filer").exists:
        current = current.parent
        if current is None:
            break
    if current: # Found a folder with a .filer in it; return it
        return current
    # Didn't find a .filer, so scan for a working copy
    working = detect_working(target)
    if working: # Found a working copy; use its .filerfrom
        return File(working.child(".filerfrom").read())
    # Didn't find a working copy; return None
    return None


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
    def __init__(self, folder, debug=None):
        if debug is None:
            debug = global_debug
        self.debug = debug
        self.folder = File(folder)
        self.filer_dir = self.folder.child(".filer")
        if not self.filer_dir.exists:
            raise Exception("There isn't a repository at %s." % folder.path)
        self.revisions = self.filer_dir.child("revisions")
        self.revisions.mkdirs(True)
        self.numbers = self.filer_dir.child("numbers")
        self.numbers.mkdirs(True)
        self.numbersbyrev = self.filer_dir.child("numbersbyrev")
        self.numbersbyrev.mkdirs(True)
        # File names are revision hashes, and their contents are BEC lists of
        # their parent revisions.
        self.changeparents = self.filer_dir.child("changeparents")
        self.changeparents.mkdirs(True)
        # File names are revision hashes, and their contents are
        # BEC lists of child revisions.
        self.changechildren = self.filer_dir.child("changechildren")
        self.changechildren.mkdirs(True)
        # File names are revision hashes, and their contents are BEC lists of
        # all commits that include this file or folder.
        self.dirparents = self.filer_dir.child("dirparents")
        self.dirparents.mkdirs(True)
        # File names are revision hashes, and their contents are BEC lists of
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
        
        The return value is a BEC object corresponding to the revision.
        
        Note that short revision numbers must still be given as strings. Bad
        things (exceptions mainly) will happen if ints are passed in instead.
        """
        if self.revisions.child(id).exists: # Revision with the same hash exists
            return bec.loads(self.revisions.child(id).read())
        if self.numbers.child(id).exists: # Revision with that number exists
            return bec.loads(self.revisions.child(self.numbers.child(id).read()).read())
        raise Exception("The revision %r does not exist." % id)
    
    def create_revision(self, data):
        """
        Creates a new revision with the specified data, which should be a BEC
        object (not a string). The new revision's hash will be returned.
        
        Entries in changeparents, changechildren, dirparents, and dirchildren
        will be created for this new revision.
        """
        if isinstance(data, dict):
            text_data = bec.dumps(data, sort_keys=True)
        else:
            text_data = data
            data = bec.loads(data)
        hash = hashlib.sha1(text_data).hexdigest()
        self.revisions.child(hash).write(text_data)
        if self.debug:
            print "Wrote revision %s" % hash
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
        self.changeparents.child(hash).write(bec.dumps(data["parents"]))
        # Write an empty changechildren file for ourselves
        self.changechildren.child(hash).write(bec.dumps([]))
        # Iterate over our parents and write ourselves into their respective
        # changechildren files
        for p in data["parents"]:
            f = self.changechildren.child(p)
            f.write(bec.dumps(bec.loads(f.read()) + [hash]))
        # If we're a file, write an empty dirchildren entry
        if data["type"] == "file":
            self.dirchildren.child(hash).write(bec.dumps([]))
        # If we're a folder, write a list of all of the hashes in our
        # "children" dict to our dirchildren entry
        elif data["type"] == "folder":
            self.dirchildren.child(hash).write(bec.dumps(data["children"].values()))
        # Write an empty dirparents file for ourselves
        self.dirparents.child(hash).write(bec.dumps([]))
        # If we're a folder, iterate over our children and write ourselves into
        # their respective dirparents files
        if data["type"] == "folder":
            for c in data["children"].values():
                f = self.dirparents.child(c)
                f.write(bec.dumps(bec.loads(f.read()) + [hash]))
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
        # If new_rev is None, just delete target, or clear it if it's a
        # directory (see a few comments below for why we do that). FIXME: We
        # might want to consider deleting it unless it's got a .filerfrom or
        # something like that to avoid it being sporadically recreated on the
        # next commit because it didn't actually go away.
        if new_rev is None:
            if target.is_folder: # Folder, so clear it
                for f in target.list():
                    if not f.name.startswith("."):
                        delete(f)
            else: # File, so delete it
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
        # Update: if the revision's a folder and target is already a folder,
        # just delete its contents instead, avoiding files that start with dots.
        # This is to avoid trampling on a working directory's .filerfrom and
        # .filerparents special files. (self.commit_changes also refuses to
        # commit files that start with a dot, so this won't lose us anything.)
        if target.exists:
            if target.is_folder and data["type"] == "folder":
                # It's a folder, and our new revision is also a folder, so
                # delete its contents that don't start with dots
                for f in target.list():
                    if not f.name.startswith("."):
                        delete(f)
            else:
                delete(target)
        # Now we check to see if we're dealing with a file or a folder.
        if data["type"] == "folder":
            # It's a folder, so we need to create a new folder for it.
            target.mkdir(silent=True)
            # Now we go iterate through the folder's children and update each
            # of them.
            for name, rev in data["children"].items():
                self.update_to(target.child(name), rev)
            # And that's it for folders.
        elif data["type"] == "file":
            # It's a file, so we just write its contents.
            target.write(data["contents"])
        # That's pretty much it for updating right now.
    
    def commit_changes(self, revstate, parent_files, new_file, info, current_name=None):
        """
        Creates a new revision with the specified revstate as parents. There
        should be one item in old_targets per item in revstate["revs"], i.e.
        one old target for every parent revision; the differences between these
        and new_target will be used as the revision to commit.
        
        If the specified revstate indicates only one parent for the current
        revision and the old target and new_target are the same, the passed-in
        revstate will be returned as-is. Otherwise, a new revstate built up
        from the newly-created revision and all of its dirchild revisions
        created for it will be returned.
        
        Note that switching types isn't supported right now; if target is a
        file in one of its parent revisions but is a folder now, or vice versa,
        bad things will happen.
        
        To commit a revision creating a file or folder that doesn't yet exist
        in the repository, simply pass in an empty list for old_targets and an
        empty revstate (i.e. {"revs": [], "children": {}}). Thus a minimal
        example of putting new content into a repository would be:
        
        from filer1.repository import Repository
        from afn.fileutils import File
        repo = Repository(File("path/to/repository/folder"))
        new_revstate = repo.commit_changes({"revs": [], "children": {}}, [],
                                           folder_to_commit, {"date":
                                           1234567890, "message":
                                           "adding some new files"})
        """
        # Make sure the old targets and the new target are all of the same type
        for index, old in enumerate(old_targets):
            if old.is_file != new_target.is_file:
            # Trying to switch types; raise an exception about it
                raise Exception("New data to commit is a %s but parent %s is "
                        "a %s" % (["folder", "file"][new_target.is_file],
                                  revstate["parents"][index],
                                  ["folder", "file"][old.is_file]))
        # They're the same type. Now see if we're a file or a folder.
        if new_target.is_file:
            # It's a file. Check to see if we've got exactly one parent and the
            # file's old contents are the same as its new contents.
            if len(old_targets) == 1 and old_target.read() == new_target.read():
                # Only one revision and the contents are the same; return the
                # revstate as-is
                return revstate
            else:
                # Multiple parents or different contents (or the file hasn't
                # been created yet), so let's go make a diff against 
            if (len(parent_revs) > 1 or
                len(parent_revs) < 1 or
                self.get_revision(parent_revs[0])["contents"] != target.read()):
                # The file's changed, or we've got more or less than just one
                # parent; create a new revision for the file and return it.
                return self.create_revision({"type": "file",
                                             "info": info,
                                             "current_name": current_name,
                                             "parents": parent_revs,
                                             "contents": target.read()})
            else:
                # The file hasn't changed and we've got only one revision.
                # Return it as-is.
                return parent_revs[0]
        else:
            # It's a folder. First thing we do is create revisions for all of
            # our children.
            child_revs = {}
            for child in target.list():
                # If the child's name starts with a dot, skip it for now. This
                # is to prevent working folder special files (.filerfrom and
                # .filerparents in particular) from being committed. TODO: work
                # out a better way to deal with this in the future, but make
                # sure to account for when a person creates a file that
                # collides with one of these special files in the repository's
                # history. Perhaps just issue a message saying that the commit
                # can't be properly checked out and should be exported instead;
                # exporting would just be a checkout without creating any
                # special files.
                if child.name.startswith("."):
                    continue                
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
                child_revs[child.name] = self.commit_changes(child_parents, child, info, child.name)
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
                                             "info": info,
                                             "current_name": current_name,
                                             "parents": parent_revs,
                                             "children": child_revs})
    
    def revision_iterator(self):
        """
        A generator that returns a (number, hash, data_str, data) tuple for all
        of the revisions in this repository. number will be a string.
        """
        current_number = 1
        while self.numbers.child(str(current_number)).exists:
            # We've still got a revision, so yield it
            hash = self.numbers.child(str(current_number)).read()
            data_str = self.revisions.child(hash).read()
            data = bec.loads(data_str)
            yield str(current_number), hash, data_str, data
            current_number += 1
    
    def number_for_rev(self, hash):
        return self.numbersbyrev.child(hash).read()
    
    def get_dirparents(self, hash):
        return bec.loads(self.dirparents.child(hash).read())
    
    def has_revision(self, hash):
        return self.revisions.child(hash).exists









