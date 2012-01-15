
# A parser/interpreter for the Fact programming language. This parser
# is a hand-coded predictive recursive descent parser.

import charstack
import exceptions
import entities
import sinks

def parse(text):
    stack = charstack.CharStack("{identity|" + text + "}")
    entity = parseFunction(stack)
    # TODO: we need to do some checking here to make sure they 
    # didn't include a premature function close or supply an
    # out-of-function "|" character
    return entity
    
def parseFunction(stack):
    if(stack.next() != "{"):
        raise exceptions.ParseException("Start of function reference must be an open brace but is not")
    argumentSequence = entities.Sequence()
    currentArgument = entities.Sequence()
    argumentSequence.add(currentArgument)
    currentLiteral = None
    while stack.more():
        c = stack.next()
        if c == "\n" or c == "\r":
            continue
        elif c == "\\":
            if currentLiteral == None :
                currentLiteral = entities.Literal()
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
            if varName.strip(): # True == Non-Whitespace
                currentArgument.append(entities.VarReference(varName))
        elif c == "{":
            currentLiteral = None
            stack.back()
            newRef = parseFunction(stack)
            currentArgument.add(newRef)
        elif c == "|":
            currentLiteral = None
            # TODO: if the current argument has only one child, replace
            # it with its child. This could improve performance a bit.
            currentArgument = entities.Sequence()
            argumentSequence.add(currentArgument)
        elif c == "}":
            currentLiteral = None
            # TODO: same TODO item about 5 lines up applies here.
            newRef = entities.FunctionReference(argumentSequence)
            return newRef
        else:
            if(currentLiteral == None):
                currentLiteral = entities.Literal()
                currentArgument.add(currentLiteral)
            currentLiteral.append(c)
    raise exceptions.ParseException('Function call not closed (IE you have more "{" than you have "}"')
            
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
        return "\x00"
    return char

    
class FactContext:
    globalVars = {}
    def __init__(self):
        self.localVars = {}
        self.subroutines = {}
        self.action = False















