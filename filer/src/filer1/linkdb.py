
from filer1 import bec

class LinkDB(object):
    def __init__(self, folder):
        self.folder = folder
    
    def link(self, source, type, target):
        link_folder = self.folder.child(type)
        link_folder.mkdirs(silent=True)
        source_file = link_folder.child(source)
    
    def get_links(self, source, type):
        pass
    
    def remove_links(self, source, type):
        pass
