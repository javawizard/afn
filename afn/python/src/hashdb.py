
from afn.fileutils import File
import sqlite3

initial_commands = """
create table if not exists nodes (
    hash text,
    type text
);
create table if not exists nodevalues (
    hash text,
    index int,
    valuetype int8,
    value
);
create table if not exists refs (
    hash text,
    index int,
    key text,
    target text
);
create index if not exists NodesByHashAndType on nodes (
    hash,
    type
);
create index if not exists NodesByTypeAndHash on nodes (
    type,
    hash
);
create index if not exists NodeValuesByHashAndIndex on nodevalues (
    hash,
    index
);
create index if not exists RefsByHashAndIndex on refs (
    hash,
    index
);
create index if not exists RefsByHashAndKey on refs (
    hash,
    key,
    index
);
"""

INTEGER = 1
TEXT = 2
BLOB = 3

def load_value(value_type, value):
    if value_type == BLOB:
        return buffer(str(value))
    else:
        return value

def store_value(value):
    if isinstance(value, basestring):
        return (TEXT, value)
    elif isinstance(value, (int, long)):
        return (INTEGER, value)
    elif isinstance(value, buffer):
        return (BLOB, buffer)
    raise TypeError

class HashDB(object):
    def __init__(self, f):
        self.db = sqlite3.connect(f.path)
        self.db.executescript(initial_commands)
    
    def get_type(self, hash):
        c = self.db.cursor()
        c.execute("select type from nodes where hash = ?", [hash])
        return c.fetchone()[0]
    
    def get_value(self, hash, default=None):
        c = self.db.cursor()
        c.execute("select valuetype, value from nodevalues where hash = ? and index = 0", [hash])
        results = c.fetchone()
        if not results:
            return default
        return load_value(*results)
    
    def get_values(self, hash):
        return self.get_value_range(hash, 0, self.get_value_count(hash))
    
    def get_value_count(self, hash):
        c = self.db.cursor()
        c.execute("select max(index) from nodevalues where hash = ?", [hash])
        return c.fetchone()[0]
    
    def get_value_range(self, hash, offset, length=1):
        c = self.db.cursor()
        c.execute("select valuetype, value from nodevalues where hash = ? order by index asc offset ? limit ?", [hash, offset, length])
        return [load_value(t, v) for t, v in c.fetchall()]
    
    def get_refs(self, hash):
        return self.get_ref_range(hash, 0, self.get_ref_count(hash))
    
    def get_ref_count(self, hash):
        c = self.db.cursor()
        c.execute("select max(index) from refs where hash = ?", [hash])
        return c.fetchone()[0]
    
    def get_ref_range(self, hash, offset, length=1):
        c = self.db.cursor()
        c.execute("select key, target from refs where hash = ? order by index asc offset ? limit ?", [hash, offset, length])
        return c.fetchall()
    
    def lookup_ref(self, hash, key, default=None):
        c = self.db.cursor()
        c.execute("select target from refs where hash = ? and key = ? and index = 0")
        results = c.fetchone()
        if not results:
            return default
        return results[0]
    
    def lookup_ref_count(self, hash, key):
        c = self.db.cursor()
        c.execute("select count(index) from refs where hash = ? and key = ?", [hash, key])
        return c.fetchone()[0]
    
    def lookup_refs(self, hash, key):
        return self.lookup_ref_range(hash, key, 0, self.lookup_ref_count(hash, key))
    
    def lookup_ref_range(self, hash, key, offset, length=1):
        c = self.db.cursor()
        c.execute("select hash from refs where hash = ? and key = ? order by index asc offset ? limit ?", [hash, key, offset, length])
        return [h for h, in c.fetchall()]
    
    def delete(self, hash):
        c = self.db.cursor()
        c.execute("delete from nodes where hash = ?", [hash])
        c.execute("delete from nodevalues where hash = ?", [hash])
        c.execute("delete from refs where hash = ?", [hash])
    
    def insert(self, value_iterable, ref_iterable):
        """
        Inserts a node. value_iterable should yield values to insert.
        ref_iterable should yield (key, hash) pairs.
        """
        values = [store_value(s) for s in value_iterable]
        refs = 
        refs = 
        c = self.db.cursor()
        c.execute("insert into nodes (hash, type) values (?, ?)", [])








