
import hashlib

def hash_file(file, algorithm=hashlib.sha1):
    """
    Creates a new instance of the specified algorithm (which defaults to
    hashlib.sha1), then updates it with the specified file-like object's
    contents. The hash object is then returned.
    
    The file's data is fed into the hash in 16KB blocks, so arbitrarily-large
    files can be fed in without causing memory issues.
    """
    hash = algorithm()
    for data in iter(lambda: file.read(16384), ""):
        hash.update(data)
    return hash

