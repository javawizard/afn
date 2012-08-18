
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
    if folder.exists:
        raise Exception("There's already a filer repository at that location.")
    folder.mkdirs(True)
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
        # Sanity check to make sure we're not using the parent folder
        if self.folder.child(".filer").exists:
            raise Exception("You should pass in the .filer folder itself to "
                    "Repository(...), not the folder that contains .filer")
        self.store_folder = self.folder.child("store")
        self.store_folder.mkdirs(True)
        self.store_folder.child("type").write("direct")
        self.store = DirectStore(self.store_folder.child("direct"))
        self.numbers = self.folder.child("numbers")
        self.numbers.mkdirs(True)
        self.numbersbyrev = self.folder.child("numbersbyrev")
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









