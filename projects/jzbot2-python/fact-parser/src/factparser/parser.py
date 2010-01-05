from entities import *
from sinks import *

def parse():
    pass

class FactContext:
    pass

class StringSink:
    def __init__(self):
        self.current = "";
    
    def write(self, new):
        self.current = self.current + new;
        
    def toString(self):
        return self.current

class FactEntity:
    def resolve(self, sink, context):
        pass
    
