
import os
import os.path

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
    
    @property
    def exists(self):
        return os.path.exists(self.path)
        
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
    
    @property
    def name(self):
        return os.path.split(self.path)[1]
    
    def recurse(self, filter=None):
        """
        A generator that recursively yields all child File objects of this file.
        Files and directories (and the files and directories contained within
        them, and so on) are all included; files for which the specified filter
        function return False (or None or some other Python false-like value)
        are not included.
        
        Note that directories for which the filter returns False are still
        recursed into. I might change this later to allow specifying whether
        the directory should not be recursed into when it's filtered out.
        """
        children = self.list()
        for child in children:
            if filter is None or filter(child):
                yield child
            if child.is_folder:
                for c in child.recurse(filter):
                    yield c
    
    def __str__(self):
        return "afn.fileutils.File(%r)" % self.path
    
    __repr__ = __str__

    
































