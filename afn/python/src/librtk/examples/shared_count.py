
from utils import print_exceptions
from librtk.various import tracker

"""
Identical to count, but the counter is shared among clients so that
clicking the button in one client causes all others to see the new value.

This example is not perfect; it has some race conditions because it doesn't
use locks where it should. I may fix this at some point, but this is
intended to be a proof-of-concept, not a perfect application.
"""

shared_count_state = 0
shared_count_clients = []

@tracker(shared_count_clients)
def start(connection):
    window = connection.Window(title="The Shared Counter")
    window.close_request.listen(connection.close)
    box = connection.VBox(window)
    label = connection.Label(box, text=str(shared_count_state))
    button = connection.Button(box, text="Click to increment the counter "
            "across all clients.")
    def clicked():
        global shared_count_state
        shared_count_state += 1
        for s_con in shared_count_clients[:]:
            with print_exceptions:
                s_con[0][0][0].text = str(shared_count_state)
    button.clicked.listen(clicked)



