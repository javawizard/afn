
from afn.utils.singleton import Singleton

broadcast_port = 52722
broadcast_interval = 10
# Between 0 and broadcast_random seconds will be randomly added to
# broadcast_interval between each broadcast. Making sure that broadcasts happen
# at random times reduces the chance of broadcasts colliding on a network
# filled with Autobus services.
broadcast_random = 5
broadcast_receiver_timeout = 1
query_initial_intervals = [0.1, 0.5, 1, 2, 4, 7]
query_response_random = 0.2 # Same as broadcast_random, but specifies up to how
# many seconds may be delayed between receiving a query and sending a response
# to that query.

constant = lambda name: Singleton("autobus2.constants.%s" % name)

SYNC = constant("SYNC")
THREAD = constant("THREAD")
ASYNC = constant("ASYNC")
# TODO: consider changing these to SERVICE_ADDED, SERVICE_UPDATED, and
# SERVICE_REMOVED for naming consistency
DISCOVERED = constant("DISCOVERED")
UNDISCOVERED = constant("UNDISCOVERED")
CHANGED = constant("CHANGED")

FUNCTION_ADDED = constant("FUNCTION_ADDED")
FUNCTION_UPDATED = constant("FUNCTION_UPDATED")
FUNCTION_REMOVED = constant("FUNCTION_REMOVED")
EVENT_ADDED = constant("EVENT_ADDED")
EVENT_UPDATED = constant("EVENT_UPDATED")
EVENT_REMOVED = constant("EVENT_REMOVED")
EVENT_FIRED = constant("EVENT_FIRED")
OBJECT_ADDED = constant("OBJECT_ADDED")
OBJECT_UPDATED = constant("OBJECT_UPDATED")
OBJECT_REMOVED = constant("OBJECT_REMOVED")
OBJECT_CHANGED = constant("OBJECT_CHANGED")
SERVICE_CHANGED = constant("SERVICE_CHANGED")





