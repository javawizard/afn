
import librtk

__doc__ = """\
This module contains a number of RTK example applications. All of them are
provided as functions that should be used as the connect function passed to
an RTK connection or server instance.
"""

def hello_world(connection):
    """
    A program that shows a window titled "Hello" containing a label whose text
    is "Hello, world! How are you?".
    """
    w = connection.Window(title="Hello")
    w.close_request.listen(connection.close)
    l = connection.Label(w, text="Hello, world! How are you?")

def multi_hello(connection):
    """
    A program that shows five labels stacked one on top of the other.
    """
    w = connection.Window(title="Hello")
    w.close_request.listen(connection.close)
    box = connection.VBox(w)
    for i in range(1,6):
        connection.Label(box, text="Hello number " + str(i))

def count(connection):
    """
    A program that shows a label with a number and a button. Every time the
    button is clicked, the number on the label increments. Multiple clients
    do not share the same number count; see shared_count for an example where
    multiple clients do.
    """
    value = [0] # Store as an array so that it's mutable from closures
    window = connection.Window(title="The Counter")
    box = connection.VBox(window)
    label = connection.Label(text=str(value[0]))
    button = connection.Button(box, text="Click to increment the counter.")
    def clicked():
        value[0] += 1
        label.text = str(value[0])
    button.clicked.listen(clicked)


def serve_example(port, name, localhost_only=True):
    """
    Starts a server serving the example with the specified name. The server,
    which is an instance of ThreadedServer, will be started and returned.
    Nothing else needs to be done to start using the example (except that you
    need to open a viewer and actually connect to the example, of course).
    
    If localhost_only is True (the default), the example will be served only
    on the loopback interface. If it's false, the example will be served on
    all interfaces.
    """
    server = librtk.ThreadedServer("127.0.0.1" if localhost_only else "", 
            port, globals()[name])
    server.start()

def run():
    """
    Calls serve_example, taking the port and name from the first two
    command-line arguments. This is intended for use with afn-python/run.
    """
    import sys
    if len(sys.argv) <= 2:
        print "You need to specify a port and an example name."
        return
    server = serve_example(int(sys.argv[0]), sys.argv[1])
    from time import sleep
    try:
        while True:
            sleep(1)
    except KeyboardInterrupt:
        print "Interrupted, shutting down"
    finally:
        server.shutdown()
        sys.exit()











