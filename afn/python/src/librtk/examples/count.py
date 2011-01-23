
"""
A program that shows a label with a number and a button. Every time the
button is clicked, the number on the label increments. Multiple clients
do not share the same number count; see shared_count for an example where
multiple clients do.
"""

def start(connection):
    value = [0] # Store as a list so that it's mutable from closures
    window = connection.Window(title="The Counter")
    window.close_request.listen(connection.close)
    box = connection.VBox(window)
    label = connection.Label(box, text=str(value[0]))
    button = connection.Button(box, text="Click to increment the counter.")
    def clicked():
        value[0] += 1
        label.text = str(value[0])
    button.clicked.listen(clicked)
