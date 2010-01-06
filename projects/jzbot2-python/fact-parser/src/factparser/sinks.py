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
        
class DelimitedSink:
    def __init__(self, sink, delimiter):
        self.sink = sink
        self.delimiter = delimiter
        self.written = False
        
    def write(self, new):
        self.sink.write(new)
        
    def next(self):
        if(self.written):
            self.sink.write(self.delimiter)
        self.written = True









