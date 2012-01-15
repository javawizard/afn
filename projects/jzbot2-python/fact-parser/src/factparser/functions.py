import random

# This file contains all of the functions present in the
# interpreter. At some point, it'd be nice to split them
# out into a number of different, categorized, files.

# Each function is present as a python function whose name
# starts with "function_", followed immediately by the
# name of the Fact function that it represents. For example,
# when the factoid parser encounters {random|...}, it calls
# the "function_random()" function in this file.

# The functions in this file take three function parameters:
#    sink:      This is a Sink (see sinks.py). You can write
#               to a factoid using the sink.
#    arguments: This is an ArgumentList (see argumentlist.py).
#               You can use it to get the arguments passed
#               to the function.
#    context:   This is the context in which the factoid is
#               running. You can use this to get local
#               variables, and in the future you will be
#               able to use this to figure out who sent the
#               factoid request, what channel the factoid is
#               currently running on, and so on.

def function_identity(sink, arguments, context):
    # This contradicts the identity function in JZBot,
    # but the difference is only apparent if a user
    # uses the function directly, which they shouldn't
    # do anyway.
    arguments.resolve(0, sink)
    
def function_ifeq(sink, arguments, context):
    if arguments.resolveString(0) == arguments.resolveString(1):
        arguments.resolve(2, sink)
    elif arguments.length() > 3:
        arguments.resolve(3, sink)
        
def function_ifneq(sink, arguments, context):
    if arguments.resolveString(0) != arguments.resolveString(1):
        arguments.resolve(2, sink)
    elif arguments.length() > 3:
        arguments.resolve(3, sink)
        
def function_lset(sink, arguments, context):
    context.localVars[arguments.resolveString(0)] = arguments.resolveString(1)

def function_random(sink, arguments, context):
    arguments.resolve(random.randint(0, arguments.length() - 1), sink)


















