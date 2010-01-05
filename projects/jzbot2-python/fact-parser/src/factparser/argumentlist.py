from parser import Sequence

class ArgumentList:
    def __init__(self, sequence, context):
        self.sequence = sequence
        self.context = context
        self.delegate = None
        self.offset = None
        self.length = None
        self.resolved = list((None,)*sequence.length())
        
    
