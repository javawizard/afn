import sinks

class ArgumentList:
    def __init__(self, sequence, context):
        self.sequence = sequence
        self.context = context
        self.delegate = None
        self.offset = None
        self.size = None
        if(sequence != None):
            self.resolved = list((None,)*sequence.size())
        
    def initAsDelegate(self, delegate, offset, length):
        self.delegate = delegate
        self.offset = offset
        self.size = length
        
    def resolve(self, index, sink):
        if self.delegate != None:
            self.delegate.resolve(self.offset + index, sink)
        else:
            self.sequence.get(index).resolve(sink, self.context)
            
    def resolveString(self, index):
        sink = sinks.StringSink()
        self.resolve(index, sink)
        return sink.toString()
    
    def getEntity(self, index):
        if self.delegate != None:
            return self.delegate.getEntity(self.offset + index)
        return self.sequence.get(index)
    
    def get(self, index, sink):
        if self.delegate != None:
            self.delegate.get(self.offset + index, sink)
        else:
            if self.resolved[index] == None:
                cache = sinks.StringSink()
                fork = sinks.ForkedSink(sink, cache)
                self.sequence.get(index).resolve(fork, self.context)
                self.resolved[index] = cache.toString()
            else:
                sink.write(self.resolved[index])

    def getString(self, index):
        sink = sinks.StringSink()
        self.get(index, sink)
        return sink.toString()
    
    def length(self):
        if(self.delegate == None):
            return self.sequence.length()
        else:
            return self.size
        
    def subList(self, offset, length= -1):
        if(length == -1):
            length = self.length() - offset
        newList = ArgumentList(None, self.context)
        newList.initAsDelegate(self, offset, length)
        return newList




















