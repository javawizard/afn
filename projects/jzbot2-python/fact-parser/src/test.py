from factparser.parser import parse
from factparser.parser import FactContext
from factparser.sinks import PrintSink, StringSink

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
# to use a StringSink to actually get the factoid's output. Note,
# however, that since the factoid parser doesn't write an extra
# newline at the end of the factoid, we have to have an empty
# "print" statement after each time we run a factoid that uses
# a PrintSink, so that each one runs on its own line.

print "If you haven't opened test.py in your text editor yet,"
print "I suggest you do so now. None of this output will make"
print "sense unless you're reading the source code in test.py."
print ""

# Demonstrate choosing a random item. Try running this multiple times.
text = "The {random|first|second|third} number was randomly chosen."
context = FactContext()
factoid = parse(text) 
factoid.resolve(PrintSink(), context)
print ""

# Demostrate how to set and get local variables. context.localVars is a dictionary.
text = "The var is %myvar% and I'm about to set a var, too. {lset|another|BYEEEEE THAR}"
context = FactContext()
context.localVars['myvar'] = "HELLOOOOOOO world!"
factoid = parse(text) 
factoid.resolve(PrintSink(), context)
print ""
print "After factoid invocation, a variable called \"another\"'s value is " + \
    context.localVars['another']

# Demonstrate use of escapes. We can include newlines using this. Also, escaping
# a space tells the factoid interpreter to act as if it weren't there. Note we're
# double-backslashing stuff since if it were only one backslash, python would
# interpret the backslash itself.
text = "This is the first line.\\nSecond line. And there's no\\ space here."
context = FactContext()
factoid = parse(text) 
factoid.resolve(PrintSink(), context)
print ""

# Demostrate the ifeq function.
text = "{ifeq|first|second|Yep.|Nope.} {ifeq|third|third|Yep.|Nope.}"
context = FactContext()
factoid = parse(text) 
factoid.resolve(PrintSink(), context)
print ""

# Demostrate the ifneq function, which is the opposite of the ifeq function.
text = "{ifneq|first|second|Yep.|Nope.} {ifneq|third|third|Yep.|Nope.}"
context = FactContext()
factoid = parse(text) 
factoid.resolve(PrintSink(), context)
print ""

# Demonstrate writing to a StringSink instead of a PrintSink, so that the
# factoid's output can be put intoa string
text = "Hello, I'm some factoid output. {ifeq|hah|hah|Yep.|Nope.}"
context = FactContext()
factoid = parse(text)
sink = StringSink()
factoid.resolve(sink, context)
result = sink.toString()
print 'The factoid\'s output was "' + result + '".'















