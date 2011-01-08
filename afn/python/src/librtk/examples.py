
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


