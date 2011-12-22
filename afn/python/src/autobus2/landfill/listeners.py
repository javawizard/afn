
from autobus2 import DISCOVERED, CHANGED, UNDISCOVERED
import time 

def print_service_listener(service_id, host, port, info, event):
    print time.ctime(time.time()) + ": SERVICE " + str(event).rpartition(".")[2] + ": " + service_id + " on " + host + ":" + str(port) + " -- " + str(info)