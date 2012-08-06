
from filer1.data.store import Store
from filer1 import bec, hashutils, exceptions
import time
import random
from afn.utils.concurrent import AtomicInteger
from afn.fileutils import File
import tempfile
import os

class DirectStore(Store):
    def __init__(self, folder):
        self.folder = folder
        folder.mkdirs(silent=True)
        self.data_dir = self.folder.child("data")
        self.data_dir.mkdirs(silent=True)
    
    def store(self, data):
        # Write the data to a temporary file
        fd, temp_name = tempfile.mkstemp()
        with os.fdopen(fd, "r+b") as opened_file:
            bec.dump(data, opened_file)
        # Hash the file
        hash = hashutils.hash_file(file)
        # We're storing things as individual files in the data folder, named
        # after the revision's hash; copy the contents into such a file.
        File(temp_name).copy_to(self.data_dir.child(hash))
        # Then delete the temporary file. TODO: Might want to have this in a
        # try/finally to make sure it always gets deleted.
        File(temp_name).delete()
    
    def get(self, hash):
        f = self.data_dir.child(hash)
        if not f.exists:
            raise exceptions.NoSuchObject(hash=hash)
        # NOTE: We specifically don't close the file here. This is a really
        # unfortunate way to do it, and I'd like to figure out if it's possible
        # to do this a different way, but the file needs to be opened for
        # streams in the resulting BEC data to be read, so we have to rely on
        # garbage collection to close the file for us once the BEC data's no
        # longer needed.
        opened_file = f.open("r+b")
        return bec.load(opened_file)


