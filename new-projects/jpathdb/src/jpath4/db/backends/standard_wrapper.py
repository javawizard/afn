
from jpath4.query import data as d


class ListWrapper(d.List):
    def __init__(self, py_list):
        self.py_list = py_list
    
    def get_size(self):
        return len(self.py_list)
    
    def get_item(self, index):
        return self.py_list[index]
    
    def get_items(self):
        return d.StandardSequence(self.py_list)


class ObjectWrapper(d.Object):
    def __init__(self, py_dict):
        self.py_dict = py_dict
    
    


