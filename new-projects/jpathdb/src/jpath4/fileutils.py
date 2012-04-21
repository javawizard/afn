
import os

def native_path(path):
    return path.replace("/", os.sep)


def internal_path(path):
    return path.replace(os.sep, "/")


class File(object):
    def __init__(self, path):
        while path.endswith("/"):
            path = path[:-1]
        self.path = path
        self.native_path = native_path(path)
    
    @property
    def is_folder(self):
        return os.path.isdir(self.native_path)
    
    @property
    def is_file(self):
        return os.path.isfile(self.native_path)
    
    def check_folder(self):
        if not self.is_folder:
            raise Exception('"%s" does not exist or is not a directory' % self.path)
    
    def check_file(self):
        if not self.is_file:
            raise Exception('"%s" does not exist or is not a file' % self.path)
    
    def list(self):
        results = os.listdir(self.native_path)
        return [File(self.path + "/" + p) for p in results]
    
    def open(self, *args, **kwargs):
        return open(self.native_path, *args, **kwargs)
    
    def child(self, path):
        return File(self.path + "/" + path)
    
    def parent(self):
        return File(internal_path(os.path.dirname(self.native_path)))

    
































