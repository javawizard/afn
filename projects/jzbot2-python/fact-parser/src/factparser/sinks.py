import specialprint

class StringSink:
    def __init__(self):
        self.current = "";
    
    def write(self, new):
        self.current = self.current + new;
        
    def toString(self):
        return self.current
    
class PrintSink:
    def write(self, new):
        specialprint.special_print(new)
        
class ForkedSink:
    def __init__(self, first, second):
        self.first = first;
        self.second = second;
        
    def write(self, new):
        self.first.write(new);
        self.second.write(new);
