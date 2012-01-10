
class Multimap(dict):
    def __init__(self):
        pass
    
    def add(self, key, value):
        if not self.key in self:
            self[key] = []
        self[key].append(value)
    
    def remove(self, key, value):
        self[key].remove(value)
        if not self[key]:
            del self[key]
    
    # TODO: add more stuff to this class