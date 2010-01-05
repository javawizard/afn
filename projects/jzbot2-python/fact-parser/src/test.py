from factparser.parser import parse
from factparser.parser import FactContext
from factparser.sinks import PrintSink

# Note that the factoid language as interpreted by marlen has
# changed since the move from marlen_jackson to multimarlen
# (the whole "{" instead of "{{" thing), so I wrote the
# interpreter to interpret the new form that only uses a 
# single "{" sign.

# All of the functions themselves are in the file
# factparser/functions.py. New functions are added simply by
# editing that file and adding new python functions following
# the naming conventions mentioned in that file.

# Anyway, here are some examples showing how to use the interpreter.
# Note the usage of PrintSink(); this constructs a new PrintSink
# which is a type of Sink that simply writes output using Python's
# print function. Later on in the file is a demonstration of how
# to use a StringSink to actually get the factoid's output.

# Demonstrate choosing a random item
text = "The {random|first|second|third} number was randomly chosen.";
context = FactContext()
factoid = parse(text); 
factoid.resolve(PrintSink(), context)

# Demostrate how to set and get local variables. context.localVars is a dictionary.
text = "The var is %myvar%. I'm about to set a var, too. {lset|another|Hey everyone}"
context = FactContext()
context.localVars['myvar'] = "This is a test local var."
factoid = parse(text); 
factoid.resolve(PrintSink(), context)
print "After factoid invocation, a variable called \"another\"'s value is " + \
    context.localVars['another']

#Demonstrate 
















