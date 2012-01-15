class CharStack:
    def __init__(self, string):
        self.string = string
        self.index = 0
        
    def next(self):
        current = self.string[self.index]
        self.index += 1
        return current;
    
    def back(self):
        if(self.index > 0):
            self.index -= 1
            
    def more(self):
        return self.index < len(self.string)
    
    def at(self):
        return self.index;
    
    def peek(self):
        return self.string[self.index]
