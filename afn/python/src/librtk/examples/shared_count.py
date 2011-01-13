
from utils import PrintExceptions

"""
Identical to count, but the counter is shared among clients so that
clicking the button in one client causes all others to see the new value.

This example is not perfect; it has some race conditions because it doesn't
use locks where it should. I may fix this at some point, but this is
intended to be a proof-of-concept, not a perfect application.
"""

shared_count_state = 0
shared_count_labels = []

def start(connection):
    global shared_count_state
    global shared_count_labels
    window = connection.Window(title="The Shared Counter")
    window.close_request.listen(connection.close)
    box = connection.VBox(window)
    label = connection.Label(box, text=str(shared_count_state))
    button = connection.Button(box, text="Click to increment the counter "
            "across all clients.")
    def clicked():
        global shared_count_state
        shared_count_state += 1
        for s_label in shared_count_labels[:]:
            with PrintExceptions():
                s_label.text = str(shared_count_state)
    button.clicked.listen(clicked)
    shared_count_labels.append(label)
    def closed():
        shared_count_labels.remove(label)
    connection.add_close_function(closed)
