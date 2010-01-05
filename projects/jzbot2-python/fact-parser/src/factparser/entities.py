class Literal:
    def __init__(self, new=""):
        self.value = new
        
    def append(self, new):
        self.value += new
        
    def resolve(self, sink, context):
        sink.write(self.value);
        
class VarReference:
    def __init__(self, name):
        self.name = name
        
    def resolve(self, sink, context):
        sink.write(context.localVars[self.name])
        
class FunctionReference:
    def __init__(self, arguments):
        self.arguments = arguments
        
class Sequence:
    def __init__(self):
        self.list = []
        
    def length(self):
        return len(self.list)
    
    def get(self, index):
        return self.list[index];
    
    def add(self, item):
        self.list.append(item);
        
    def remove(self, index):
        del self.list[index]
        
    def resolve(self, sink, context):
        for item in self.list:
            item.resolve(sink, context)
        









