
class Singleton(object):
    def __init__(self, name):
        self.name = name
        self.short_name = name.rpartition(".")[2]
    
    def __str__(self):
        return self.name
    
    def __repr__(self):
        return self.name
