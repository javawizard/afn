from autobus2 import Bus
import sys

bus = Bus()
connection = bus.connect(sys.argv[1], sys.argv[2], sys.argv[3])
connection["hi"]()
connection.close()
bus.close()
