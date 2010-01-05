import random

# This file contains all of the functions present in the
# interpreter. At some point, it'd be nice to split them
# out into a number of different, categorized, files.

def function_identity(sink, arguments, context):
    # This contradicts the identity function in JZBot,
    # but the difference is only apparent if a user
    # uses the function directly, which they shouldn't
    # do anyway.
    arguments.resolve(0, sink)
    
def function_lset(sink, arguments, context):
    context.localVars[arguments.resolveString(0)] = arguments.resolveString(1)

def function_random(sink, arguments, context):
    arguments.resolve(random.randint(0, arguments.length() - 1), sink)
