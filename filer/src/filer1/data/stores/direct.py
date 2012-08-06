
from filer1.data.store import Store
from filer1 import bec
import time
import random
from afn.utils.concurrent import AtomicInteger
from afn.fileutils import File

class DirectStore(Store):
    def __init__(self, folder):
        self.folder = folder
        folder.mkdirs(silent=True)
        self.data_dir = self.folder.child("data")
        self.data_dir.mkdirs(silent=True)
        self.temp_dir = self.folder.child("temp")
        self.temp_dir.mkdirs(silent=True)
        self.temp_sequence = AtomicInteger(1)
    
    def store(self, data):
        f = self._generate_temp_file()
        with f.open("r+b") as opened_file:
            bec.dump(data, opened_file)
        
    
    def get(self, hash):
        pass
    
    def _generate_temp_file(self):
        # Create a temporary file with a relatively random name, one basically
        # guaranteed not to cause any conflicts. I'd like to use Python's
        # tempfile module for this, but I can't find any decent way to create
        # a file that can be later renamed and used as an ordinary file without
        # all of, for example, the weird permissions that mkfstemp puts on the
        # file in question.
        name = "%s-%s-%s" % (time.time(), self.temp_sequence.get_and_add(),
                random.random())
        return self.temp_dir.child(name)
        


