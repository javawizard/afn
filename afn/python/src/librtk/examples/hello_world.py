
"""
A program that shows a window titled "Hello" containing a label whose text
is "Hello, world! How are you?".
"""

def start(connection):
    w = connection.Window(title="Hello")
    w.close_request.listen(connection.close)
    l = connection.Label(w, text="Hello, world! How are you?")
