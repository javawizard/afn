
from afn.fileutils import File
import json
from filer1 import repository

XATTR_BASE = "user.filer-base"
XATTR_REPO = "user.filer-repo"

class WorkingCopy(object):
    def __init__(self, repository, working):
        self.repository = repository
        self.working = working
    
    def commit(self, info, target=None, current_name=None):
        """
        Commits changes. TODO: Document this better.
        """
        if target is None:
            target = self.working
        # FIXME: Check to make sure our parents are of the same type as we are,
        # and figure out what to do if they're not. (Perhaps create a new
        # file/folder without any parents?)
        if target.get_xattr(XATTR_BASE) is None:
            # No extended attribute is present, which means the file hasn't
            # been scheduled to be committed. So we just return.
            return
        base = json.loads(target.get_xattr(XATTR_BASE, "null"))
        # Let's get started. First we see if we're a file or a folder.
        if target.is_file:
            # It's a file. Check to see if we've got exactly one parent and the
            # file's old contents are the same as its new contents.
            if len(base) == 1:
                with target.open("rb") as f:
                    if repository.same_contents(f,self.get_revision(
                            base[0])["contents"]):
                        # Only one revision and the contents are the same;
                        # just return
                        return
            # The file's changed, or we've got more or less than just one
            # parent; create a new revision for the file, then update its
            # xattr to reflect the new parent. FIXME: We need to somehow make
            # the updating of xattrs atomic over the whole changeset.
            with target.open("rb") as f:
                hash = self.create_revision({"type": "file",
                                             "info": info,
                                             "current_name": current_name,
                                             "parents": base,
                                             "contents": f})
                target.set_xattr(XATTR_BASE, json.dumps([hash]))
        else:
            # It's a folder. First thing we do is create revisions for all of
            # our children. Note that we call commit for all of our children,
            # even ones that shouldn't be committed; commit will filter out
            # those that don't have a XATTR_BASE attribute for us.
            for child in target.list():
                # We're ignoring symbolic links for now, but we'll print a
                # friendly warning
                if child.is_link:
                    print ("Warning: symbolic link %r will be ignored. "
                            "Support for symbolic links will be present in "
                            "the future." % child.path)
                    continue
                # Now we create a revision for the specified child.
                self.commit_changes(info, child, child.name)
            # Now we iterate over our children and read their parent hashes
            # into child_revs, which will then map child names to corresponding
            # child revision hashes.
            child_revs = {}
            for child in target.list():
                # Only add children that are actually being tracked
                if child.get_xattr(XATTR_BASE) is not None:
                    # The comma just before the equals sign is intentional.
                    # Don't delete it.
                    child_revs[child.name], = json.loads(child.get_xattr(XATTR_BASE))
            # We've got a dictionary of child revisions. Now we check to see if
            # we've got exactly one parent, and it has the exact same revisions
            # we do; if that's the case, we just return the current revstate.
            if (len(base) == 1 and
                self.get_revision(base[0])
                        ["contents"] == child_revs):
                # Same child revs, so we just return
                return
            else:
                # Different child revs or multiple parents, so we create a new
                # revision for this folder and update its xattr. FIXME: The
                # same issue about atomically updating the xattrs that applies
                # to files applies here.
                hash = self.create_revision({"type": "folder",
                                             "info": info,
                                             "current_name": current_name,
                                             "parents": base,
                                             "contents": child_revs})
                target.set_xattr(XATTR_BASE, json.dumps([hash]))
    
