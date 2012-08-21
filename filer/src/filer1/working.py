
from afn.fileutils import File
import json
from filer1 import repository
import shutil
from filer1.constants import XATTR_BASE, XATTR_REPO


def delete_tracked(target):
    """
    Deletes the specified target if it's tracked and, if it's a folder, doesn't
    have any untracked children (or formerly-tracked children that themselves
    have untracked children, and so on). Folders that do have untracked
    children will themselves be untracked instead of being deleted.
    
    Update: files/folders that have a XATTR_REPO will be untracked but not
    deleted, to avoid steamrollering over the working copy root.
    """
    if target.is_link:
        # It's a link. We'll treat it as an untracked file for now and give it
        # no further consideration.
        pass
    elif target.is_file:
        # It's a file. See if it's tracked.
        if target.has_xattr(XATTR_BASE):
            # It's tracked, so see if it has XATTR_REPO.
            if target.has_xattr(XATTR_REPO):
                # It does, so untrack it but don't delete it.
                target.delete_xattr(XATTR_REPO)
            else:
                # It doesn't, so delete it.
                target.delete()
        else:
            # It's not tracked, so leave it as it is.
            pass
    elif target.is_folder:
        # It's a folder. See if it's tracked.
        if target.has_xattr(XATTR_BASE):
            # It's tracked. Iterate over its children and recursively call
            # delete_tracked.
            target.delete_xattr(XATTR_BASE)
            for child in target.list():
                delete_tracked(child)
            # We've already untracked it; now see if it has zero children and
            # doesn't have XATTR_REPO.
            if len(target.list()) == 0 and not target.has_xattr(XATTR_REPO):
                # No contents and no XATTR_REPO, so delete it
                target.delete(True)
        else:
            # It's not tracked, so leave it as it is.
            pass
    else:
        raise ValueError("%r is not a file or a folder" % target)


class WorkingCopy(object):
    def __init__(self, repository, working):
        self.repository = repository
        self.working = working
    
    def create(self):
        if self.working.has_xattr(XATTR_REPO):
            raise Exception("%r is already a working copy." % self.working)
        path = self.repository.folder.path
        # Workaround to make repositories that are immediate children of their
        # working copies use relative paths
        if self.repository.folder.parent == self.working:
            path = self.repository.folder.name
        self.working.set_xattr(XATTR_REPO, path)
    
    def is_working(self):
        return self.working.exists and self.working.has_xattr(XATTR_REPO)
    
    def update_to(self, new_rev, target=None):
        """
        Updates target to the revision specified by new_rev. The target's
        extended attributes will be created or update as necessary.
        
        Note: Right now, this only deletes directories which have no untracked
        files. Directories that do have untracked files will simply be
        untracked themselves instead of deleted.
        """
        if target is None:
            target = self.working
        # new_rev can't be None, as we shouldn't get this far if it is
        if new_rev is None:
            raise Exception("Can't specify None as the revision to update to")
        # new_rev isn't None, so we need to update to it. First we need to
        # make sure we've got a revision hash and not a revision number.
        if self.repository.numbers.child(new_rev).exists:
            new_rev = self.repository.numbers.child(new_rev).read()
        # Then we read the revision's data.
        data = self.repository.get_revision(new_rev)
        # Then we delete the target so that we can start off with a clean slate.
        # Obviously we need to do something a bit better once we get past the
        # prototype stage.
        # Update: if the revision's a folder and target is already a folder,
        # just delete its tracked contents instead. This will avoid
        # steamrollering over untracked files in the folder.
        if target.exists:
            delete_tracked(target)
        # Now we check to see if we're dealing with a file or a folder.
        if data["type"] == "folder":
            # It's a folder, so we need to create a new folder for it, if it
            # doesn't already exist (due to having untracked files, as above)
            target.mkdir(silent=True)
            # Then we temporarily untrack the folder if it exists so that if
            # we die, we don't see a corrupted view of the folder's current
            # revision, if it already exists
            target.delete_xattr(XATTR_BASE)
            # Now we go iterate through the folder's children and update each
            # of them.
            for name, rev in data["contents"].items():
                self.update_to(rev, target.child(name))
            # Then we track the folder again
            target.set_xattr(XATTR_BASE, json.dumps([new_rev]))
            # And that's it.            
        elif data["type"] == "file":
            # It's a file. We seek the file to zero (as coming from who knows
            # where, its position could be all over the board) and then copy
            # the file's contents into the target. We untrack the file while
            # updating it for the same reason that we untrack folders while
            # updating them.
            target.delete_xattr(XATTR_BASE)
            data["contents"].seek(0)
            with target.open("wb") as target_stream:
                shutil.copyfileobj(data["contents"], target_stream)
            target.set_xattr(XATTR_BASE, json.dumps([new_rev]))
        # That's pretty much it for updating right now.
    
    def commit(self, info, target=None, current_name=None):
        """
        Commits changes. TODO: Document this better.
        """
        if target is None:
            target = self.working
        # FIXME: Check to make sure our parents are of the same type as we are,
        # and figure out what to do if they're not. (Perhaps create a new
        # file/folder without any parents?)
        if not target.has_xattr(XATTR_BASE):
            # No extended attribute is present, which means the file hasn't
            # been scheduled to be committed. So we just return. TODO: This
            # presents a race condition: what if the attribute is deleted
            # elsewhere between being checked and being read here? Although
            # if that's the case, other things that will screw up our logic
            # even worse are likely to happen, so maybe not that big of a deal.
            return
        base = json.loads(target.get_xattr(XATTR_BASE, "null"))
        # Let's get started. First we see if we're a file or a folder.
        if target.is_file:
            # It's a file. Check to see if we've got exactly one parent and the
            # file's old contents are the same as its new contents.
            if len(base) == 1:
                with target.open("rb") as f:
                    if repository.same_contents(f, self.repository.get_revision(
                            base[0])["contents"]):
                        # Only one revision and the contents are the same;
                        # just return
                        return
            # The file's changed, or we've got more or less than just one
            # parent; create a new revision for the file, then update its
            # xattr to reflect the new parent. FIXME: We need to somehow make
            # the updating of xattrs atomic over the whole changeset.
            with target.open("rb") as f:
                hash = self.repository.create_revision({"type": "file",
                                             "info": info,
                                             "current_name": current_name,
                                             "parents": base,
                                             "contents": f})
                self.print_commit_info(target, hash, base)
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
                self.commit(info, child, child.name)
            # Now we iterate over our children and read their parent hashes
            # into child_revs, which will then map child names to corresponding
            # child revision hashes.
            child_revs = {}
            for child in target.list():
                if child.is_link:
                    continue
                # Only add children that are actually being tracked
                if child.get_xattr(XATTR_BASE) is not None:
                    # The comma just before the equals sign is intentional.
                    # Don't delete it.
                    child_revs[child.name], = json.loads(child.get_xattr(XATTR_BASE))
            # We've got a dictionary of child revisions. Now we check to see if
            # we've got exactly one parent, and it has the exact same revisions
            # we do; if that's the case, we just return the current revstate.
            if (len(base) == 1 and
                self.repository.get_revision(base[0])
                        ["contents"] == child_revs):
                # Same child revs, so we just return
                return
            else:
                # Different child revs or multiple parents, so we create a new
                # revision for this folder and update its xattr. FIXME: The
                # same issue about atomically updating the xattrs that applies
                # to files applies here.
                hash = self.repository.create_revision({"type": "folder",
                                             "info": info,
                                             "current_name": current_name,
                                             "parents": base,
                                             "contents": child_revs})
                self.print_commit_info(target, hash, base)
                target.set_xattr(XATTR_BASE, json.dumps([hash]))
    
    def print_commit_info(self, target, hash, base):
        if len(base) == 0:
            code = "A"
        elif len(base) == 1:
            code = "M"
        else:
            code = "G"
        print code + " " + hash + " " + target.relative_path(self.working)
    
