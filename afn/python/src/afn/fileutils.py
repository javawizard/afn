"""
An object-oriented file access library for Python.

More module documentation to come soon. For now, take a look at the File class
to see what it does.
"""

# This is afn.fileutils inside the AFN project, but it's used just as fileutils
# nearly everywhere else. Name it what you will.
# The authoritative version of fileutils can be found at:
# http://hg.opengroove.org/afn/file/default/afn/python/src/afn/fileutils.py

import os
import os.path
import shutil
import zipfile as zip_module
from contextlib import closing
import errno
import urllib
import hashlib
from functools import partial as _partial

SKIP = "skip"
RECURSE = "recurse"
YIELD = "yield"


def file_or_none(f):
    """
    Returns File(f), unless f is None, in which case None is returned.
    """
    if f is not None:
        return File(f)
    else:
        return None


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
        r"""
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
        # Make the pathname absolute, and normalize case
        path = os.path.abspath(os.path.normcase(path))
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
        
        If this file is a symbolic link that points to a file, this will be
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
        even for broken symbolic links; use self.valid if you want an
        alternative that returns False for broken symbolic links.
        """
        return os.path.lexists(self._path)
    
    @property
    def valid(self):
        """
        True if this file/folder exists, False if it doesn't. This will be
        False for broken symbolic links; use self.exists if you want an
        alternative that returns True for broken symbolic links.
        """
        return os.path.exists(self._path)
        
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
    
    @property
    def children(self):
        """
        A list of all of the children of this file, as a list of File objects.
        If this file is not a folder, the value of this property is None.
        """
        if not self.is_folder:
            return
        return [self.child(p) for p in self.child_names]
    
    def list(self):
        """
        An obsolete method that simply returns self.children.
        """
        return self.children
    
    @property
    def child_names(self):
        """
        A list of the names of all of the children of this file, as a list of
        strings. If this file is not a folder, the value of this property is
        None.
        """
        if not self.is_folder:
            return
        return sorted(os.listdir(self._path))
    
    def list_names(self):
        """
        An obsolete method that simply returns self.child_names.
        """
        return self.child_names
    
    def open(self, *args, **kwargs):
        """
        Opens the file referred to by this File object and returns a Python
        file instance. *args and **kwargs are passed through to Python's open
        function as if by open(self.path, *args, **kwargs).
        """
        return open(self._path, *args, **kwargs)
    
    def child(self, *names):
        """
        Returns a File object representing the child of this file with the
        specified name. If multiple names are present, they will be joined
        together. If no names are present, self will be returned.
        
        If any names are absolute, all names before them (and self) will be
        discarded. Relative names (like "..") are also allowed. If you want a
        method that guarantees that the result is a child of self, use
        self.safe_child(...).
        
        This method is analogous to os.path.join(self.path, *names).
        """
        return File(os.path.join(self.path, *names))
    
    def safe_child(self, *names):
        """
        Same as self.child(*names), but checks that the resulting file is a
        descendant of self. If it's not, an exception will be thrown. This
        allows unsanitized paths to be used without fear that things like ".."
        will be used to escape the confines of self.
        
        The pathname may contain occurrences of ".." so long as they do not
        escape self. For example, "a/b/../c" is perfectly fine, but "a/../.."
        is not.
        """
        child = self.child(*names)
        if not self.ancestor_of(child):
            raise ValueError("Names %r escape the parent %r" % (names, self))
        return child
    
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
    
    def ancestors(self, including_self=False):
        """
        Returns a list of all of the ancestors of this file, with self.parent
        first. If including_self is True, self will be first, self.parent will
        be second, and so on.
        """
        if including_self:
            current = self
        else:
            current = self.parent
        results = []
        while current is not None:
            results.append(current)
            current = current.parent
        return results
    
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
        
        The same warning about the path property applies here.
        """
        return self._path.split(os.path.sep)
    
    def copy_to(self, other, overwrite=False):
        """
        Copies the contents of this file to the specified File object or
        pathname. An exception will be thrown if the specified file already
        exists and overwrite is False.
        
        This does not currently work for folders; I hope to add this ability
        in the near future.
        """
        other = File(other)
        self.check_file()
        if other.exists and not overwrite:
            raise Exception("%r already exists" % other)
        with other.open("wb") as write_to:
            for block in self.read_blocks():
                write_to.write(block)
    
    def download_from(self, url, overwrite=False):
        """
        Downloads the specified URL and saves it to self. If self already
        exists and overwrite is False, an exception will be thrown; otherwise,
        self will be overwritten.
        """
        if self.exists and not overwrite:
            raise Exception("%r already exists" % self)
        urllib.urlretrieve(url, self._path)
    
    def recurse(self, filter=None, include_self=True, recurse_skipped=True):
        """
        A generator that recursively yields all child File objects of this file.
        Files and directories (and the files and directories contained within
        them, and so on) are all included.
        
        A filter function accepting one argument can be specified. It will be
        called for each file and folder. It can return one of True, False,
        SKIP, YIELD, or RECURSE, with the behavior of each outlined in the
        following table:
            
                                Don't yield   Do yield
                               +------------+---------+
            Don't recurse into | SKIP       | YIELD   |
                               +------------+---------+
            Do recurse into    | RECURSE    | True    |
                               +------------+---------+
            
            False behaves the same as RECURSE if recurse_skipped is True, or
            SKIP otherwise.
        
        If include_self is True (the default), this file (a.k.a. self) will be
        yielded as well (if it matches the specified filter function). If it's
        False, only this file's children (and their children, and so on) will
        be yielded.
        """
        include = True if filter is None else filter(self)
        if include in (YIELD, True) and include_self:
            yield self
        if include in (RECURSE, True) or (recurse_skipped and not include):
            for child in self.children() or []:
                child.recurse(filter, True, recurse_skipped)
        
    @property
    def size(self):
        """
        The size, in bytes, of this file. This is the number of bytes that the
        file contains; the number of actual bytes of disk space it consumes is
        usually larger.
        
        If this file is actually a folder, the sizes of its child files and
        folders will be recursively summed up and returned. This can take quite
        some time for large folders.
        
        This is the same as len(self).
        """
        if self.is_folder:
            return sum(f.size for f in self.children)
        elif self.is_file:
            return os.path.getsize(self._path)
        else: # Broken symbolic link or some other type of file
            return 0
    
    def __len__(self):
        return self.size
    
    def rename_to(self, other):
        """
        Rename this file or folder to the specified name, which can be a File
        object or a pathname.
        """
        os.rename(self._path, File(other).path)
    
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
    
    def read_blocks(self, block_size=16384, binary=True):
        """
        A generator that yields successive blocks of data from this file. Each
        block will be no larger than block_size bytes, which defaults to 16384.
        This is useful when reading/processing files larger than would
        otherwise fit into memory.
        
        One could implement, for example, a copy function thus:
        
        with target.open("wb") as target_stream:
            for block in source.read_blocks():
                target_stream.write(block)
        """
        with self.open("r" + ("b" if binary else "")) as f:
            data = f.read(block_size)
            while data:
                yield data
                data = f.read(block_size)
    
    def hash(self, algorithm=hashlib.md5, return_hex=True):
        """
        Compute the hash of this file and return it, as a hexidecimal string.
        
        The default algorithm is md5. An alternate constructor from hashlib
        can be passed as the algorithm parameter; file.hash(hashlib.sha1)
        would, for example, compute the SHA-1 hash instead.
        
        If return_hex is False (it defaults to True), the hash object itself
        will be returned instead of the return value of its hexdigest() method.
        One can use this to access the binary hash instead.
        """
        hasher = algorithm()
        with self.open("rb") as f:
            for block in iter(_partial(f.read, 10), ""):
                hasher.update(block)
        if return_hex:
            hasher = hasher.hexdigest()
        return hasher
    
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
        two alternate spellings in common use.
        """
        self.mkdirs(*args, **kwargs)
    
    def change_to(self):
        """
        Sets the current working directory to self.
        
        Since File instances internally store paths in absolute form, other
        File instances will continue to work just fine after this is called.
        
        If you need to restore the working directory at any point, you might
        want to consider using self.as_working instead.
        """
        os.chdir(self._path)
    
    def cd(self):
        """
        An alias for self.change_to().
        """
        self.change_to()
    
    @property
    def as_working(self):
        """
        A property that returns a context manager. This context manager sets
        the working directory to self upon being entered and restores it to
        what it previously was upon being exited. One can use this to replace
        something like:
        
        old_dir = os.getcwd() # A fileutils version of this will be coming soon
        new_dir.cd()
        try:
            ...stuff...
        finally:
            File(old_dir).cd() # Or os.chdir(old_dir)
        
        with the much nicer:
        
        with new_dir.as_working:
            ...stuff...
        
        and get exactly the same effect.
        
        The context manager's __enter__ returns self (this file), so you can
        also use an "as" clause on the with statement to get access to the
        file in case you haven't got it stored in a variable anywhere.
        """
        return _AsWorking(self)
    
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
        with closing(zip_module.ZipFile(File(filename).path, "w")) as zipfile:
            for f in self.recurse():
                if contents:
                    path_in_zip = f.relative_path(self)
                else:
                    path_in_zip = f.relative_path(self.parent)
                zipfile.write(f.path, path_in_zip)
        
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
            zipfile.extractall(folder.path)
    
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
        """
        if relative_to is None:
            relative_to = File().path
        return os.path.relpath(self._path, relative_to.path)
    
    def ancestor_of(self, other):
        """
        Returns true if this file is an ancestor of the specified file. A file
        is an ancestor of another file if that other file's parent is this
        file, or its parent's parent is this file, and so on.
        
        Note that if self == other, False will be returned.
        """
        parent = File(other).parent
        while parent is not None:
            if parent == self:
                return True
            parent = parent.parent
        return False
    
    def descendant_of(self, other):
        """
        Returns true if this file is a descendant of the specified file. This
        is equivalent to File(other).ancestor_of(self).
        """
        return File(other).ancestor_of(self)
    
    def link_to(self, other):
        """
        Creates this file as a symbolic link pointing to other, which can be
        a pathname or a File object. Note that if it's a pathname, a symbolic
        link will be created with the exact path specified; it will therefore
        be absolute if the path is absolute or relative (to the link itself) if
        the path is relative. If a File object, however, is used, the symbolic
        link will always be absolute.
        """
        if isinstance(other, File):
            os.symlink(other.path, self._path)
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
        
        Setting extended attributes on symlinks will result in undefined and
        platform-specific behavior. Don't do it unless you know what you're
        doing.
        
        Extended attribute support currently requires the PyPI module "xattr"
        to be installed. An exception will be thrown if it is not available.
        You can usually install it by running "sudo pip install xattr".
        
        NOTE: For now, if xattr support is not enabled, this will just return
        the empty list. Try using set_xattr if you want to get an exception if
        extended attributes aren't enabled. I'll probably add a method for
        checking if extended attributes are enabled on the file in question
        later on.
        """
        import xattr
        try:
            return list(xattr.listxattr(self.path))
        except IOError as e:
            if e.errno == errno.EOPNOTSUPP or e.errno == errno.ENOENT: # No
                # xattr support or the file doesn't exist
                return []
            else:
                raise
    
    def set_xattr(self, name, value):
        """
        Sets an extended attribute with the specified name and value on this
        file.
        
        The same compatibility warnings present on list_xattrs apply here.
        """
        import xattr
        xattr.setxattr(self.path, name, value)
    
    def get_xattr(self, name, default=None):
        """
        Returns the value of the extended attribute with the specified name
        on this file, as a string, or returns the value of default (which
        defaults to None) if no such extended attribute exists.
        
        The same compatibility warnings present on list_xattrs apply here.
        """
        import xattr
        try:
            return xattr.getxattr(self.path, name)
        except IOError as e:
            # TODO: See if this is different on other platforms, such as OS X
            if (e.errno == errno.ENODATA or e.errno == errno.EOPNOTSUPP
                    or e.errno == errno.ENOENT):
                return default
            else:
                raise
    
    def has_xattr(self, name):
        """
        Returns True if this file has an extended attribute with the specified
        name, or False if it doesn't.
        
        The same compatibility warnings present on list_xattrs apply here.
        """
        return self.get_xattr(name, None) is not None
    
    def check_xattr(self, name):
        """
        Same as get_xattr, but throws a KeyError if the specified extended
        attribute does not exist instead of returning a default value.
        
        The same compatibility warnings present on list_xattrs apply here.
        
        Note that an IOError will be thrown if extended attributes aren't
        supported instead of KeyError.
        """
        import xattr
        try:
            return xattr.getxattr(self.path, name)
        except IOError as e:
            if e.errno == errno.ENODATA:
                raise KeyError("File %r has no attribute %r" % (self.path, name))
            else:
                raise
    
    def delete_xattr(self, name, silent=True):
        """
        Deletes the extended attribute with the specified name from this file.
        If the attribute does not exist and silent is true, nothing will
        happen. If the attribute does not exist and silent is false, an
        exception will be thrown.
        
        The same compatibility warnings present on list_xattrs apply here.
        """
        import xattr
        try:
            xattr.removexattr(self.path, name)
        except IOError as e:
            if (e.errno == errno.ENODATA or e.errno == errno.EOPNOTSUPP
                    or e.errno == errno.ENOENT) and silent: # Attribute doesn't exist
                # and silent is true; do nothing.
                pass
            else:
                raise
    
    def __str__(self):
        return "fileutils.File(%r)" % self._path
    
    __repr__ = __str__
    
    # Use __cmp__ instead of the rich comparison operators for brevity
    def __cmp__(self, other):
        if not isinstance(other, File):
            return NotImplemented
        return cmp(self.path, other.path)
    
    def __nonzero__(self):
        """
        Returns True. File objects are always true values; to test for their
        existence, use self.exists instead.
        """
        return True


class _AsWorking(object):
    """
    The class of the context managers returned from File.as_working. See that
    method's docstring for more information on what this class does.
    """
    def __init__(self, folder):
        self.folder = folder
    
    def __enter__(self):
        self.old_path = os.getcwd()
        self.folder.cd()
        return self.folder
    
    def __exit__(self, *args):
        os.chdir(self.old_path)

    
































