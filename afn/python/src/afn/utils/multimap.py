
class Multimap(dict):
    def __init__(self, on_create=None, on_delete=None):
        self.on_create = on_create
        self.on_delete = on_delete
    
    def add(self, key, value):
        created = False
        if not key in self:
            self[key] = []
            created = True
        self[key].append(value)
        if created:
            if self.on_create is not None:
                self.on_create(key)
    
    def remove(self, key, value):
        self[key].remove(value)
        if not self[key]:
            del self[key]
            if self.on_delete is not None:
                self.on_delete(key)
    
    # TODO: add more stuff to this class