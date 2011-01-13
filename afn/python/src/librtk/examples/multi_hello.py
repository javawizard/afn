
"""
A program that shows five labels stacked one on top of the other.
"""

def start(connection):
    w = connection.Window(title="Hello")
    w.close_request.listen(connection.close)
    box = connection.VBox(w)
    for i in range(1,6):
        connection.Label(box, text="Hello number " + str(i))
