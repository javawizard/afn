from charstack import *
from entities import *
from sinks import *

def parse(text):
    stack = CharStack("{identity|" + text + "}")
    entity = parseFunction(stack)
    # TODO: we need to do some checking here to make sure they 
    # didn't include a premature function close or supply an
    # out-of-function "|" character
    return entity
    
def parseFunction(stack):
    if(stack.next() != "{"):
        raise ParseException("Start of function reference must be an open brace but is not")
    argumentSequence = Sequence()
    currentArgument = Sequence()
    argumentSequence.add(currentArgument)
    currentLiteral = None
    while stack.more():
        c = stack.next();
        if c == "\n" or c == "\r":
            continue
        elif c == "\\":
            if(currentLiteral == None):
                currentLiteral = Literal()
    
class FactContext:
    globalVars = {}
    def __init__(self):
        self.localVars = {}
        self.subroutines = {}
        self.action = False

class ParseException(Exception):
    def __init__(self, reason):
        self.reason = reason;
        
    def __str__(self):
        return "Syntax exception while parsing factoid: " + self.reason















