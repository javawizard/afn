
import sqlite3
from afn.fileutils import File
from contextlib import closing
import hashlib

def init_repository(folder):
    folder = File(folder)
    db_file = folder.child(".filerdata")
    if db_file.exists:
        raise Exception("There's already a filer repository at that location.")
    # Don't use transactions for now. This is a bad idea and needs to be fixed
    # in the future.
    db = sqlite3.connect(db_file.native_path, isolation_level=None)
    c = db.cursor()
    c.execute("create table revisions (hash text unique, data text, number integer primary key autoincrement)")
    db.close()


class Repository(object):
    def __init__(self, folder):
        self.folder = File(folder)
        self.db_file = self.folder.child(".filerdata")
        # Don't use transactions for now. This is a bad idea and needs to be
        # fixed in the future.
        self.db = sqlite3.connect(self.db_file.native_path, isolation_level=None)
    
    def __enter__(self):
        pass
    
    def __exit__(self, *args):
        self.db.close()
    
    def get_revision(self, id):
        """
        Gets the revision with the specified revision id, which can either be a
        hex string representing the full sha1 hash or the revision's numeric id.
        A ValueError will be thrown if no such revision exists.
        """
        with closing(self.db.cursor()) as c:
            data = c.execute("select data from revisions where hash = ?", str(id))
            if data is None:
                data = c.execute("select data from revisions where number = ?",
                        int(id))
            if data is None:
                raise ValueError("No such revision: %r" % id)
            return data
    
    def create_revision(self, data):
        """
        Creates a new revision with the specified data. A (hash, number) tuple
        will be returned.
        """
        hash = hashlib.sha1(data).hexdigest()
        with closing(self.db.cursor()) as c:
            c.execute("insert into revisions (hash, data) values (?, ?)",
                    hash, data)
            return hash, c.lastrowid
            









