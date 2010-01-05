from charstack import *
from exceptions import ParseException
from entities import *
from sinks import *
from string import strip

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
        c = stack.next()
        if c == "\n" or c == "\r":
            continue
        elif c == "\\":
            if currentLiteral == None :
                currentLiteral = Literal()
                currentArgument.add(currentLiteral)
            theChar = getEscapedChar(stack.next())
            if theChar == "[":
                v = stack.next()
                while v != "]":
                    currentLiteral.append(v)
                    v = stack.next()
            elif theChar != "\x00":
                currentLiteral.append(theChar)
        elif c == "%":
            currentLiteral = None
            varName = ""
            c = stack.next()
            while  c != "%":
                varName = varName + c
                c = stack.next()
            if strip(varName) != "":
                currentArgument.append(VarReference(varName))
        elif c == "{":
            currentLiteral = None
            stack.back()
            newRef = parseFunction(stack)
            currentArgument.add(newRef)
        elif c == "|":
            currentLiteral = None
            # TODO: if the current argument has only one child, replace
            # it with its child. This could improve performance a bit.
            currentArgument = Sequence()
            argumentSequence.add(currentArgument)
        elif c == "}":
            currentLiteral = None
            # TODO: same TODO item about 5 lines up applies here.
            newRef = FunctionReference(argumentSequence)
            return newRef
        else:
            if(currentLiteral == None):
                currentLiteral = Literal()
                currentArgument.add(currentLiteral)
            currentLiteral.append(c)
    raise ParseException('Function call not closed (IE you have more "{" than you have "}"')
            
def getEscapedChar(char):
    if char == "n":
        return "\n"
    elif char == "r":
        return "\r"
    elif char == "p":
        return "\x0f"
    elif char == "b":
        return "\x02"
    elif char == "u":
        return "\x1f"
    elif char == "i":
        return "\x16"
    elif char == "c":
        return "\x03"
    elif char == "x" or char == " ":
        return 0
    return char

    
class FactContext:
    globalVars = {}
    def __init__(self):
        self.localVars = {}
        self.subroutines = {}
        self.action = False















