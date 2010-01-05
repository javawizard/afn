from argumentlist import ArgumentList
from parser import ParseException
import functions

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
        
    def resolve(self, sink, context):
        list = ArgumentList(self.arguments, context)
        functionName = list.getString(0)
        function = getattr(functions, "function_" + functionName)
        if(function == None):
            #TODO: iirc this is redundant, since I think getattr throws an
            # exception if there's no such attribute
            raise ParseException("No such function: " + functionName);
        subList = list.subList(1)
        function(sink, subList, context);
        
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
        









