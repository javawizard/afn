
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
        if new_rev is None. The target's new revstate will be returned.
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
            # FIXME: What are we supposed to return here? In my tired stupor
            # I'm returning an empty revstate, but I'm not sure if that's
            # exactly right...
            return {"parents": [], "children": {}}
        # new_rev isn't None, so we need to update to it. First we need to
        # make sure we've got a revision hash and not a revision number.
        if self.numbers.child(new_rev).exists:
            new_rev = self.numbers.child(new_rev).read()
        # Then we read the revision's data.
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
            # of them, keeping track of the resulting revstates.
            child_revstates = {}
            for name, rev in data["contents"].items():
                child_revstates[name] = self.update_to(target.child(name), rev)
            # Then we return a revstate.
            return {"parents": [new_rev], "children": child_revstates}                
        elif data["type"] == "file":
            # It's a file. We seek the file to zero (as coming from who knows
            # where, its position could be all over the board) and then copy
            # the file's contents into the target.
            data["contents"].seek(0)
            shutil.copyfileobj(data["contents"], target)
            # Then we return a revstate.
            return {"parents": [new_rev], "children": {}}
        # That's pretty much it for updating right now.
    
    def revision_iterator(self):
        """
        A generator that returns a (number, hash, data_str, data) tuple for all
        of the revisions in this repository. number will be a string.
        """
        current_number = 1
        while self.numbers.child(str(current_number)).exists:
            # We've still got a revision, so yield it
            hash = self.numbers.child(str(current_number)).read()
            data = self.store.get(hash)
            yield str(current_number), hash, data
            current_number += 1
    
    def number_for_rev(self, hash):
        """
        Returns the short number (as a string) of the specified revision hash.
        """
        return self.numbersbyrev.child(hash).read()
        
    def has_revision(self, hash):
        """
        Returns True if the specified revision is present in this repository.
        """
        return self.store.has(hash)









