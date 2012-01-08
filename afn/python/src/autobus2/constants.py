
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