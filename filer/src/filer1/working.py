
from afn.fileutils import File

class WorkingCopy(object):
    def __init__(self, repository, target):
        self.repository = repository
        self.target = target
    
    