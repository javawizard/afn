
from afn.fileutils import File
import json
from filer1 import repository

XATTR_BASE = "user.filer-base"
XATTR_REPO = "user.filer-repo"

class WorkingCopy(object):
    def __init__(self, repository, working):
        self.repository = repository
        self.working = working
    
    TODO: Pick up here August 18, 2012. Convert the updater to use xattrs
    instead of revstates. Have it delete folders that are being deleted and
    that have no contents, and untrack those that do. (Untracking is done by 
    removing the XATTR_BASE extended attribute.)
    
    def update_to(self, target, new_rev):
        """
        Updates target to the revision specified by new_rev, or deletes target
        if new_rev is None. The target's extended attributes will be created
        or updated as necessary.
        
        Note: Right now, this only deletes directories which have no untracked
        files. Directories that do have untracked files will simply be
        untracked themselves instead of deleted.
        """
        # If new_rev is None, the target's been removed. If it's a file, we
        # delete it. If it's a folder, we remove all of its tracked contents,
        # then delete it if it's empty or untrack it if it's not. FIXME: This
        # will break if a file's been changed to a folder between the current
        # revision and the one we're updating to; figure out what to do then.
        # (Perhaps scan the directory structure beforehand and warn the user
        # if any folders would be slated to be changed to files that have
        # untracked contents.)
        if new_rev is None:
            if target.is_folder: # Folder, so delete its untracked contents
                for child in target.list():
                    if 
            else: # File, so delete it
                target.delete()
            # FIXME: What are we supposed to return here? In my tired stupor
            # I'm returning an empty revstate, but I'm not sure if that's
            # exactly right...
            return {"parents": [], "children": {}}
        # new_rev isn't None, so we need to update to it. First we need to
        # make sure we've got a revision hash and not a revision number.
        if self.numbers.child(new_rev).exists:
            new_rev = self.numbers.child(new_rev).read()
        # Then we read the revision's data.
        data = self.get_revision(new_rev)
        # Then we delete the target so that we can start off with a clean slate.
        # Obviously we need to do something a bit better once we get past the
        # prototype stage. TODO: Modify fileutils to just use one function for
        # deleting things; I've written stuff down on why I kept things as two
        # separate methods, but I've decided I want them together, as it'll get
        # rid of a bunch of if/else statements like I've got here.
        # Update: if the revision's a folder and target is already a folder,
        # just delete its contents instead, avoiding files that start with dots.
        # This is to avoid trampling on a working directory's .filerfrom and
        # .filerparents special files. (self.commit_changes also refuses to
        # commit files that start with a dot, so this won't lose us anything.)
        if target.exists:
            if target.is_folder and data["type"] == "folder":
                # It's a folder, and our new revision is also a folder, so
                # delete its contents that don't start with dots
                for f in target.list():
                    if not f.name.startswith("."):
                        delete(f)
            else: # Changing types or not a folder, so delete it
                delete(target)
        # Now we check to see if we're dealing with a file or a folder.
        if data["type"] == "folder":
            # It's a folder, so we need to create a new folder for it.
            target.mkdir(silent=True)
            # Now we go iterate through the folder's children and update each
            # of them, keeping track of the resulting revstates.
            child_revstates = {}
            for name, rev in data["contents"].items():
                child_revstates[name] = self.update_to(target.child(name), rev)
            # Then we return a revstate.
            return {"parents": [new_rev], "children": child_revstates}                
        elif data["type"] == "file":
            # It's a file. We seek the file to zero (as coming from who knows
            # where, its position could be all over the board) and then copy
            # the file's contents into the target.
            data["contents"].seek(0)
            shutil.copyfileobj(data["contents"], target)
            # Then we return a revstate.
            return {"parents": [new_rev], "children": {}}
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
        print code + " " + hash + " " + target.path.relative_to(self.target)
    
