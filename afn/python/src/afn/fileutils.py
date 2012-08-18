"""
Fileutils is an object-oriented filesystem library for Python.

More module documentation to come soon. For now, take a look at the File class
to see what it does.
"""

import os
import os.path
import shutil
import zipfile as zip_module
from contextlib import closing


class File(object):
    """
    An object representing a file or folder. File objects are intended to be as
    opaque as possible; one should rarely, if ever, need to know about the
    pathname of a File object, or that a File even has a pathname associated
    with it.
    
    The file or folder referred to by a File object need not exist. One can
    test whether a File object represents a file that does exist using the
    exists property.
    
    File objects cannot be changed to refer to a different file after they are
    created.
    """
    def __init__(self, *path_components):
        """
        Creates a new file from the specified path components. Each component
        represents the name of a folder or a file. These are internally joined
        as if by os.path.join(*path_components).
        
        It's also possible, although not recommended, to pass a full pathname
        (in the operating system's native format) into File. On Windows, one
        could therefore do File(r"C:\some\file"), and File("/some/file") on
        Linux and other Unix operating systems.
        
        You can also call File(File(...)). This is equivalent to File(...) and
        exists to make it easier for functions to accept either a pathname or
        a File object.
        
        Passing no arguments (i.e. File()) results in a file that refers to the
        working directory as of the time the File instance was constructed.
        
        Pathnames are internally stored in absolute form; as a result, changing
        the working directory after creating a File instance will not change
        the file referred to.
        """
        # If we're passed a File object, use its path
        if path_components and isinstance(path_components[0], File):
            path = path_components[0]._path
        # Join the path components, or use the empty string if there are none
        elif path_components:
            path = os.path.join(*path_components)
        else:
            path = ""
        # Make the pathname absolute
        path = os.path.abspath(path)
        self._path = path
    
    @property
    def is_folder(self):
        """
        True if this File is a folder, False if it isn't. If the file/folder
        doesn't actually exist yet, this will be False.
        
        If this file is a symbolic link that points to a folder, this will be
        True.
        """
        return os.path.isdir(self._path)
    
    @property
    def is_directory(self):
        """
        Same as self.is_folder.
        """
        return self.is_folder
    
    @property
    def is_file(self):
        """
        True if this File is a file, False if it isn't. If the file/folder
        doesn't actually exist yet, this will be False.
        
        If this file is a symbolic link that points to a folder, this will be
        True.
        """
        return os.path.isfile(self._path)
    
    @property
    def is_link(self):
        """
        True if this File is a symbolic link, False if it isn't. This will be
        True even for broken symbolic links.
        """
        return os.path.islink(self._path)
    
    @property
    def is_broken(self):
        """
        True if this File is a symbolic link that is broken, False if it isn't.
        """
        return self.is_link and not os.path.exists(self._path)
    
    @property
    def exists(self):
        """
        True if this file/folder exists, False if it doesn't. This will be True
        even for broken symbolic links.
        """
        return os.path.lexists(self._path)
        
    def check_folder(self):
        """
        Checks to see whether this File refers to a folder. If it doesn't, an
        exception will be thrown.
        """
        if not self.is_folder:
            raise Exception('"%s" does not exist or is not a directory' % self._path)
    
    def check_file(self):
        """
        Checks to see whether this File refers to a file. If it doesn't, an
        exception will be thrown.
        """
        if not self.is_file:
            raise Exception('"%s" does not exist or is not a file' % self._path)
    
    def list(self):
        """
        Returns all of the children of this file, as a list of File objects. If
        this file is not a folder, an exception will be thrown.
        """
        self.check_folder()
        results = os.listdir(self._path)
        return [self.child(p) for p in results]
    
    def list_names(self):
        """
        Returns all of the children of this file, as a list of strings
        representing each child's name. This is roughly equivalent to
        [f.name for f in self.list()], except that no intermittent File objects
        are created.
        """
        self.check_folder()
        return os.listdir(self._path)
    
    def open(self, *args, **kwargs):
        """
        Opens the file referred to by this File object and returns a Python
        file instance. *args and **kwargs are passed through to Python's open
        function as if by open(self.path, *args, **kwargs).
        """
        return open(self._path, *args, **kwargs)
    
    def child(self, name):
        """
        Returns a File object representing the child of this file with the
        specified name.
        """
        return File(self._path, name)
    
    def sibling(self, name):
        """
        Returns a File object representing the sibling of this file with the
        specified name. This is equivalent to self.parent.child(name).
        """
        return self.parent.child(name)
    
    @property
    def parent(self):
        """
        Returns a File representing the parent of this file. If this file has
        no parent (for example, if it's "/" on Unix-based operating systems or
        a drive letter on Windows), None will be returned.
        """
        dirname = os.path.dirname(self._path)
        # I don't remember at the moment how this works on Windows, so cover
        # all the bases until I can test it out
        if dirname is None or dirname == "":
            return None
        f = File(dirname)
        # Linux returns the same file from dirname
        if f == self:
            return None
        return f
    
    @property
    def name(self):
        """
        The name of this file. For example, File("a", "b", "c").name will be
        "c".
        
        On Unix-based operating systems, File("/").name will be the empty
        string.
        """
        return os.path.basename(self._path)
    
    @property
    def path(self):
        """
        The pathname of this File object, in a format native to the operating
        system in use. This pathname can then be used with Python's traditional
        file-related utilities.
        """
        return self._path
    
    @property
    def path_components(self):
        """
        A list of the components of the pathname of this file. This is
        currently the same as self.path.split(os.path.sep).
        """
        return self._path.split(os.path.sep)
    
    def copy_to(self, other):
        """
        Copies the contents of this file to the specified File object or
        pathname.
        
        This does not currently work for folders; I hope to add this ability
        in the near future.
        """
        other = File(other)
        self.check_file()
        shutil.copyfile(self._path, other._path)
    
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
    
    @property
    def size(self):
        """
        The size, in bytes, of this file. This is the number of bytes that the
        file contains; the number of actual bytes of disk space it consumes is
        usually larger.
        
        This is the same as len(self).
        """
        return os.path.getsize(self._path)
    
    def __len__(self):
        return self.size
    
    def rename_to(self, other):
        """
        Rename this file or folder to the specified name, which can be a File
        object or a pathname.
        """
        os.rename(self._path, File(other)._path)
    
    def read(self, binary=True):
        """
        Read the contents of this file and return them as a string. This is
        usually a bad idea if the file in question is large, as the entire
        contents of the file will be loaded into memory.
        
        If binary is True (the default), the file will be read byte-for-byte.
        If it's False, the file will be read in text mode.
        """
        with self.open("r" + ("b" if binary else "")) as f:
            return f.read()
    
    def write(self, data, binary=True):
        """
        Overwrite this file with the specified data. After this is called,
        self.size will be equal to len(data), and self.read() will be equal to
        data. If you want to append data instead, use self.append().
        
        If binary is True (the default), the file will be written
        byte-for-byte. If it's False, the file will be written in text mode. 
        """
        with self.open("w" + ("b" if binary else "")) as f:
            f.write(data)
    
    def append(self, data, binary=True):
        """
        Append the specified data to the end of this file.
        
        If binary is True (the default), the file will be appended to
        byte-for-byte. If it's False, the file will be appended to in text
        mode.
        """
        with self.open("a" + ("b" if binary else "")) as f:
            f.write(data)
    
    def mkdir(self, silent=False):
        """
        Creates the folder referred to by this File object. If it already
        exists but is not a folder, an exception will be thrown. If it already
        exists and is a folder, an exception will be thrown if silent is
        False (the default); if silent is True, no exception will be thrown.
        """
        if self.is_folder:
            if silent:
                return
            else:
                raise Exception("The folder %r already exists." % self._path)
        else:
            os.mkdir(self._path)
    
    def mkdirs(self, silent=False):
        """
        Same as self.mkdir, but creates parent directories as needed if they do
        not already exist.
        """
        if self.is_folder:
            if silent:
                return
            else:
                raise Exception("The folder %r already exists." % self._path)
        else:
            os.makedirs(self._path)
    
    def makedirs(self, *args, **kwargs):
        """
        Same as self.mkdirs(*args, **kwargs). Exists mainly because there are
        two alternate spellings in use.
        """
        self.mkdirs(*args, **kwargs)
    
    def zip_into(self, filename, contents=True):
        """
        Creates a zip archive of this folder and writes it to the specified
        filename, which can be either a pathname or a File object.
        
        If contents is True (the default), the files (and folders, and so on
        recursively) contained within this folder will be written directly to
        the zip file. If it's False, the folder will be written itself. The
        difference is that, given a folder foo which looks like this:
        
        foo/
            bar
            baz/
                qux
        
        Specifying contents=False will result in a zip file whose contents look
        something like:
        
        zipfile.zip/
            foo/
                bar
                baz/
                    qux
        
        Whereas specifying contents=True will result in this:
        
        zipfile.zip/
            bar
            baz/
                qux
        
        NOTE: This has only been tested on Linux. I still need to test it on
        Windows to make sure pathnames are being handled correctly.
        """
        with closing(zip_module.ZipFile(File(filename)._path, "w")) as zipfile:
            for f in self.recurse():
                if contents:
                    path_in_zip = f.relative_path(self)
                else:
                    path_in_zip = f.relative_path(self.parent)
                zipfile.write(f._path, path_in_zip)
        
    def unzip_into(self, folder):
        """
        Unzips the zip file referred to by self into the specified folder,
        which will be automatically created (as if by File(folder).mkdirs())
        if it does not yet exist.
        
        NOTE: This is an unsafe operation! The same warning present on Python's
        zipfile.ZipFile.extractall applies here, namely that a maliciously
        crafted zip file could cause absolute filesystem paths to be
        overwritten. I hope to hand-roll my own extraction code in the future
        that will explicitly filter out absolute paths.
        
        The return value of this function is File(folder).
        """
        folder = File(folder)
        folder.mkdirs(silent=True)
        with closing(zip_module.ZipFile(self._path, "r")) as zipfile:
            zipfile.extractall(folder._path)
    
    def delete(self, contents=False):
        """
        Deletes this file.
        
        If this is a non-empty folder, contents must be True in order to
        recursively delete the folder's contents; if it's False, an exception
        will be thrown instead. This is intended mostly as a sanity check to
        prevent folders with contents from being unintentionally deleted.
        """
        if self.is_file or self.is_link:
            os.remove(self._path)
        else:
            if contents:
                shutil.rmtree(self._path)
            else:
                os.rmdir(self._path)
    
    def delete_folder(self, contents=False):
        """
        This has been merged into the delete method and as such should no
        longer be used. It has the exact same effect as delete.
        """
        self.delete(contents)
    
    def relative_path(self, relative_to=None):
        """
        Returns the path of self, relative to the specified File object or
        filename. The idea is that:
        
        File("a/b/c", some_file.relative_path("a/b/c"))
        
        would be the same as some_file.
        
        If relative_to is not specified, the working directory is used instead.
        
        NOTE: right now, this only works if relative_to is an ancestor of self.
        This is a limitation that will be removed soon.
        """
        if relative_to is None:
            relative_to = File()
        relative_to = File(relative_to)
        if not relative_to.ancestor_of(self):
            raise NotImplementedError("Relativizing paths against non-"
                    "ancestor folders has not yet been implemented, and %r "
                    " is not an ancestor of %r." % (relative_to, self))
        if not self._path.startswith(relative_to._path):
            raise Exception("Paths appear to be ancestor/descendant properly, "
                    "but self.path doensn't start with relative_to.path. The "
                    "former and the latter are %r and %r, respectively." %
                    (self, relative_to))
        return self._path[len(relative_to._path):]
    
    def ancestor_of(self, other):
        """
        Returns true if this file is an ancestor of the specified file. A file
        is an ancestor of another file if that other file's parent is this
        file, or its parent's parent is this file, and so on.
        
        Note that if self == other, False will be returned.
        """
        parent = File(other).parent
        count = 3000
        while parent is not None:
            # Sanity check to make sure we don't get into infinite loops,
            # because I'm paranoid about the correctness of my implementation
            # of the parent property when the file in question has no parents
            count -= 1
            if count < 0:
                raise Exception("Probable infinite loop in %r.ancestor_of(%r)"
                        % (self, other))
            if parent == self:
                return True
            parent = parent.parent
        return False
    
    def descendant_of(self, other):
        """
        Returns true if this file is a descendant of the specified file. This
        is equivalent to File(other).is_ancestor(self).
        """
        return File(other).ancestor_of(self)
    
    def link_to(self, other):
        """
        Creates this file as a symbolic link pointing to other, which can be
        a pathname or a File object. Note that if it's a pathname, a symbolic
        link will be created with the exact path specified; it will therefore
        be absolute if the path is absolute or relative if the path is relative.
        If a File object, however, is used, the symbolic link will always be
        absolute.
        """
        if isinstance(other, File):
            os.symlink(other._path, self._path)
        else:
            os.symlink(other, self._path)
    
    @property
    def link_target(self):
        """
        Returns the target to which this file, which is expected to be a
        symbolic link, points, as a string. If this file is not a symbolic
        link, None is returned.
        """
        if not self.is_link:
            return None
        return os.readlink(self._path)
    
    def list_xattrs(self):
        """
        Returns a list of the names of all of the extended attributes present
        on this file.
        
        This is only supported on Linux and Max OS X right now. I hope to add
        Windows support in the future.
        
        Note that some Linux filesystems must be mounted with a particular
        option to enable extended attribute support. Ext2/3/4, for example,
        must be mounted with the user_xattr mount option set. (I heard
        somewhere that user_xattr might become a default option in the near
        future, but such a default is certainly not widely deployed yet.)
        
        Also note that some filesystems (such as ext2/3/4) require the
        attribute name to have a particular format (in the case of ext2/3/4,
        "prefix.name", where "prefix" is one of "system", "trusted",
        "security", and "user"). An "Operation not supported" error will be
        produced if an invalid format is used.
        
        Extended attribute support currently requires the PyPI module "xattr"
        to be installed. An exception will be thrown if it is not available.
        """
    
    def set_xattr(self, name, value):
        """
        Sets an extended attribute with the specified name and value on this
        file.
        
        The same compatibility warnings present on list_xattrs apply here.
        """
    
    def get_xattr(self, name, default=None):
        """
        Returns the value of the extended attribute with the specified name
        on this file, as a string, or returns the value of default (which
        defaults to None) if no such extended attribute exists.
        
        The same compatibility warnings present on list_xattrs apply here.
        """
    
    def check_xattr(self, name):
        """
        Same as get_xattr, but throws a KeyError if the specified extended
        attribute does not exist instead of returning a default value.
        
        The same compatibility warnings present on list_xattrs apply here.
        """
    
    def delete_xattr(self, name):
        """
        Deletes the extended attribute with the specified name from this file.
        This function silently does nothing if no such attribute exists.
        
        The same compatibility warnings present on list_xattrs apply here.
        """
    
    def __str__(self):
        return "fileutils.File(%r)" % self._path
    
    __repr__ = __str__
    
    def __eq__(self, other):
        if not isinstance(other, File):
            return NotImplemented
        return self._path == other._path
    
    def __ne__(self, other):
        if not isinstance(other, File):
            return NotImplemented
        return self._path != other._path

    
































