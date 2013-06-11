
"""
(This is still a work in progress, and doesn't actually work yet.)

Library similar to subprocess but that should fix a number of issues I have
with it.

For instance, the existence of subprocess.call/check_call/check_output as
separate from, say, methods on subprocess's Popen class always drove me nuts.
processutils's Process class will have the equivalent functionality present as
methods that can be called.

I also think it would be fun to write some classes and functions to implement
shell-like things such as pipes, stream redirection (to/from files), perhaps an
easy way to yield lines from a process, and so on. And maybe even have it log
when things are invoked, although I really don't like Python's logging module
so I might skip that for now.

So the idea is that one can do things like:

    Process([...]).wait().check().get_output()

which will wait until the process is done, check to make sure it succeeded
(and throw an exception if it exited with anything other than 0), then return
its stdout and stderr as strings.

(I might have check() implicitly wait() as well, as there's not much point to
doing anything else.)

Things will return self where possible to facilitate easy chaining of calls as
needed.
"""

class Process(object):
    def __init__(self, command, environment):
        raise NotImplementedError
    
    def wait(self):
        # Wait for this process to finish, then return self
        pass
    
    def check(self):
        # Throw exception if nonzero exit code, otherwise return self
        pass
    
    def get_output(self):
        # Return stdout and stderr, maybe have a separate function for each
        # and have one that returns both
        pass
    
    @property
    def return_code(self):
        pass
    
    def get_code_and_output(self):
        # Returns stdout, stderr, and return_code
        pass



