from autobus2 import Bus
import sys
from time import sleep

bus = Bus()
connection = bus.connect(sys.argv[1], int(sys.argv[2]), sys.argv[3])
connection["hi"]()
connection["hi"]("and hello again!")
connection["hi"]("what's your name?")
sleep(2)
connection["hi"]("good-bye!")
connection.close()
bus.close()
