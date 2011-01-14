
from __future__ import with_statement
import librtk
import librtk.protocols

"""
This package contains a number of RTK example applications. All of them are
provided as modules that contain one important function, start, which should
be passed as the on_connect function to an RTK connection.

This module's serve_example function can be used to automatically start up a
server serving a particular named example.
"""


def serve_example(port, name, localhost_only=True):
    """
    Creates a server that can serve the specified example. This function itself
    does not start the server; you need to call the return value's start
    function to start the server. The server will be a ThreadedServer, so if
    you'd like to run the accept loop on the current thread you can call the
    run function on the return value instead of the start function.
    
    If localhost_only is True (the default), the example will be served only
    on the loopback interface. If it's false, the example will be served on
    all interfaces.
    """
    __import__("librtk.examples." + name)
    import sys
    return librtk.protocols.ThreadedServer("127.0.0.1" if localhost_only else "", 
            port, sys.modules["librtk.examples." + name].start)

def main():
    """
    Calls serve_example, taking the port and name from the first two
    command-line arguments. This is intended for use with afn-python/run.
    
    The third command-line argument, which is optional and should be true or
    false (the default), specifies whether or not the example should be
    accessible outside of the local host.
    """
    import sys
    if len(sys.argv) <= 2:
        print "You need to specify a port and an example name."
        return
    server = serve_example(int(sys.argv[1]), sys.argv[2],
            localhost_only=len(sys.argv) > 3 and sys.argv[3] == "true")
    try:
        server.run()
    except KeyboardInterrupt:
        print "Interrupted, shutting down"
    finally:
        server.shutdown()
        sys.exit()











