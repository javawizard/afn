
class Multimap(dict):
    def __init__(self, on_create=None, on_delete=None, on_before_create=None,
            on_before_delete=None):
        self.on_create = on_create
        self.on_delete = on_delete
        self.on_before_create = on_before_create
        self.on_before_delete = on_before_delete
    
    def add(self, key, value):
        created = False
        if not key in self:
            if self.on_before_create:
                self.on_before_create()
            self[key] = []
            created = True
        self[key].append(value)
        if created:
            if self.on_create is not None:
                self.on_create(key)
    
    def remove(self, key, value):
        if self[key] == [value]:
            if self.on_before_delete:
                self.on_before_delete()
        self[key].remove(value)
        if not self[key]:
            del self[key]
            if self.on_delete is not None:
                self.on_delete(key)
    
    # TODO: add more stuff to this class