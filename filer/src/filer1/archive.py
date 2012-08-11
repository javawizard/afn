

MAGIC = "filer-archive-1:"
from threading import Thread, RLock, current_thread
import bsdiff4
import gzip
import shutil
from afn.fileutils import File
from filer1 import bec
from afn.utils.concurrent import AtomicInteger

LITERAL = "\x01"
LITERAL_GZIP = "\x02"
DIFF = "\x03"
DIFF_GZIP = "\x04"

def compress_folder(folder, out, temp_dir):
    objects, bec_map = load_objects(folder)
    sort_commit_objects(objects, bec_map)
    make_archive(objects, out, temp_dir)


def load_objects(folder):
    objects = []
    for f in folder.list():
        objects.append((f.name, f))
    bec_map = {}
    for h, f in objects:
        bec_map[h] = bec.load(f.open("rb"))
    return objects, bec_map


def sort_commit_objects(objects, bec_map):
#    objects.sort(key=lambda a: bec_map.get("current_name", ""))
    objects.sort(key=lambda a: a[1].size)
    objects.reverse()


def make_archive(objects, out, temp_dir, thread_count=8):
    """
    Objects is a list of (hash, fileutils.File) tuples representing the objects
    to encode, in order.
    """
    temp_dir.mkdirs(True)
    # out.write(MAGIC)
    current = AtomicInteger(0)
    # A lock that's acquired by every thread before choosing an object to
    # compress
    in_lock = RLock()
    # TODO: Spawn several processor threads
    # So, what we're doing is writing each compressed object to the compress
    # folder. Then we combine them together afterward.
    compress_folder = temp_dir.child("compress")
    if compress_folder.is_folder:
        compress_folder.delete_folder(True)
    compress_folder.mkdirs()
    in_total, out_total = AtomicInteger(0), AtomicInteger(0)
    def run():
        i, o = _process(objects, current, in_lock, temp_dir)
        in_total.get_and_add(i)
        out_total.get_and_add(o)
    threads = [Thread(target=run) for i in range(thread_count)]
    for t in threads:
        t.start()
    for t in threads:
        t.join()
    in_total, out_total = int(in_total), int(out_total)
    print "Total space savings: %s%%" % int(100*(1.0 - (float(out_total) / float(in_total))))
    print "Input size: %sKB, output size: %sKB" % (in_total / 1000, out_total / 1000)
    print len(objects)


def _process(objects, current, in_lock, temp_dir):
    in_total, out_total = 0, 0
    while True:
        position = current.get_and_add()
        if position >= len(objects):
            return in_total, out_total
        # print "Starting %s in %s" % (position, current_thread())
        hash, object_file = objects[position]
        window_copy = objects[max(0, position - 70):position]
        o_dir = temp_dir.child(hash)
        o_dir.mkdirs()
        try:
            # First, generate diffs and gzipped diffs
            for h, f in window_copy:
                p = o_dir.child(h)
                # Generate diff
                bsdiff4.file_diff(f.path, object_file.path, p.path)
                # Generate gzipped diff
#                g = o_dir.child(h + ".gz")
#                with open(p.path, "rb") as in_patch:
#                    with gzip.GzipFile(filename=g.path, mode="wb") as out_patch:
#                        shutil.copyfileobj(in_patch, out_patch)
            # Then gzip the original
            with object_file.open("rb") as in_file:
                with gzip.GzipFile(filename=o_dir.child("original.gz").path, mode="wb") as out_file:
                    shutil.copyfileobj(in_file, out_file)
            # And compare the files.
            # Gather the files to compare
            to_compare = []
            for h, f in window_copy:
                p = o_dir.child(h)
                to_compare.append((p.size, DIFF, p))
#                g = o_dir.child(h + ".gz")
#                to_compare.append((g.size, DIFF_GZIP, g))
            to_compare.append((object_file.size, LITERAL, object_file))
            object_file_g = o_dir.child("original.gz")
            to_compare.append((object_file_g.size, LITERAL_GZIP, object_file_g))
            # Compare them
            min_size, min_type, min_file = min(to_compare)
            # Print a message about how much we compressed
            original_size = object_file.size
            fraction_of_original = float(min_size) / float(original_size)
            amount_saved = 1.0 - fraction_of_original
#            print "For object %s:" % hash
#            print "original_size, min_size, min_type, min_file: %s, %s, %s, %s" % (
#                    original_size, min_size, hex(ord(min_type)), min_file)
#            print "Compressed to %s%% smaller (larger numbers are better)" % (
#                    int(amount_saved * 100))
            in_total += original_size
            out_total += min_size
            print "Compressed %s" % position
#            print "Space savings so far: %s%%" % int(100*(1.0 - (float(out_total) / float(in_total))))
        finally:
            o_dir.delete_folder(True)

















