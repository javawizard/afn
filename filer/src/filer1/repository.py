
import sqlite3
from afn.fileutils import File
from contextlib import closing
import hashlib
from filer1 import exceptions
from filer1 import bec
from filer1.data.stores.direct import DirectStore
import shutil

global_debug = False

def same_contents(fp1, fp2):
    """
    Compares the contents of two file-like objects to see if they're the same.
    Both files will be seeked to zero before comparing, and both will be left
    at indeterminate positions once a difference is found.
    """
    fp1.seek(0)
    fp2.seek(0)
    while True:
        d1 = fp1.read(8192)
        d2 = fp2.read(8192)
        if d1 != d2:
            return False
        if not d1: # and not d2, since if they were different, we wouldn't get
            # here because of the return statement on the previous line
            return True


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
        self.store_folder = self.filer_dir.child("store")
        self.store_folder.mkdirs(True)
        self.store_folder.child("type").write("direct")
        self.store = DirectStore(self.store_folder.child("direct"))
        self.numbers = self.filer_dir.child("numbers")
        self.numbers.mkdirs(True)
        self.numbersbyrev = self.filer_dir.child("numbersbyrev")
        self.numbersbyrev.mkdirs(True)
    
    def get_revision(self, id):
        """
        Gets the revision with the specified revision id, which can either be a
        hex string representing the full sha1 hash or the revision's numeric id.
        An exceptions.NoSuchObject will be thrown if no such revision exists.
        
        The return value is a BEC object corresponding to the revision.
        
        Note that short revision numbers must still be given as strings. Bad
        things (exceptions mainly) will happen if ints are passed in instead.
        """
        if self.numbers.child(id).exists:
            id = self.numbers.child(id).read()
        return self.store.get(id)
    
    def create_revision(self, data):
        """
        Creates a new revision with the specified data, which should be a BEC
        object (not a string). The new revision's hash will be returned.
        
        Entries in changeparents, changechildren, dirparents, and dirchildren
        will be created for this new revision. (Update: those have been
        disabled for now, and will probably be replaced with some sort of graph
        database soon.)
        """
        if not isinstance(data, dict):
            raise Exception("Invalid revision data value: %r (must be a dict)"
                    % data)
        hash = self.store.store(data)
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
        return hash
    
    def update_to(self, target, new_rev):
        """
        Updates target to the revision specified by new_rev, or deletes target
        if new_rev is None.
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
            else: # Changing types or not a folder, so delete it
                delete(target)
        # Now we check to see if we're dealing with a file or a folder.
        if data["type"] == "folder":
            # It's a folder, so we need to create a new folder for it.
            target.mkdir(silent=True)
            # Now we go iterate through the folder's children and update each
            # of them.
            for name, rev in data["contents"].items():
                self.update_to(target.child(name), rev)
            # And that's it for folders.
        elif data["type"] == "file":
            # It's a file, so we go seek the file to zero, then stream the
            # file's contents into the target.
            data["contents"].seek(0)
            shutil.copyfileobj(data["contents"], target)
        # That's pretty much it for updating right now.
    
    def commit_changes(self, revstate, target, info, current_name=None):
        """
        Commits changes. TODO: Document this better.
        """
        # FIXME: Check to make sure our parents are of the same type as we are,
        # and figure out what to do if they're not. (Perhaps create a new
        # file/folder without any parents?)
        # Anyways, let's get started. First we see if we're a file or a folder.
        if target.is_file:
            # It's a file. Check to see if we've got exactly one parent and the
            # file's old contents are the same as its new contents.
            if len(revstate["parents"]) == 1:
                with target.open("rb") as f:
                    if same_contents(f,self.get_revision(
                            revstate["parents"][0])["contents"]):
                        # Only one revision and the contents are the same;
                        # return the revstate as-is
                        return revstate
            # The file's changed, or we've got more or less than just one
            # parent; create a new revision for the file and return a
            # corresponding revstate.
            with target.open("rb") as f:
                hash = self.create_revision({"type": "file",
                                             "info": info,
                                             "current_name": current_name,
                                             "parents": revstate["parents"],
                                             "contents": f})
                return {"parents": [hash], "children": {}}
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
                # We used to build up a list of parents for this child from the
                # folder's current revision, but we're using revstates now to
                # allow merges to add new parents to a child. So we just need
                # to go get a revstate for this particular child, or use an
                # empty one if it doesn't exist in the revstate we were
                # provided.
                child_revstate = revstate["children"].get(child.name)
                if not child_revstate:
                    child_revstate = {"parents": [], "children": {}}
                # Now we create a revision for the specified child.
                child_revs[child.name] = self.commit_changes(
                        child_revstate, child, info, child.name)
            # child_revs will map child names to revstates; we need to expand
            # this into hashes (newly-created revstates will always mention
            # exactly one parent hash, namely the one just created) to compare
            # with the child revisions of our current revision
            child_rev_hashes = dict((n, r["parents"][0]) for n, r in child_revs.items())
            # We've got a dictionary of child revisions. Now we check to see if
            # we've got exactly one parent, and it has the exact same revisions
            # we do; if that's the case, we just return the current revstate.
            if (len(revstate["parents"]) == 1 and
                self.get_revision(revstate["parents"][0])
                        ["contents"] == child_rev_hashes):
                # Same child revs, so we return the revstate as-is
                return revstate
            else:
                # Different child revs or multiple parents, so we create a new
                # revision for this folder and return a revstate for it.
                hash = self.create_revision({"type": "folder",
                                             "info": info,
                                             "current_name": current_name,
                                             "parents": revstate["parents"],
                                             "contents": child_rev_hashes})
                return {"parents": [hash], "children": child_revs}
    
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









