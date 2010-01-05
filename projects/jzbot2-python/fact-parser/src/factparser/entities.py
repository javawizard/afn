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
