
from parallel import Parallel
from autobus2 import Bus
from time import sleep
import time

class Sink(object):
    def __init__(self):
        self.service_dict = {}
    
    def receive_change(self, connection, info, name, old, new):
        if new is None:
            if name in self.service_dict:
                del self.service_dict[name]
        else:
            self.service_dict[name] = new
        self.compute_and_write()
    
    def compute_and_write(self):
        print "Before flush: %s" % time.ctime()
        state_dict = {}
        for states in self.service_dict.values():
            for k, v in states.iteritems():
                state_dict[k] = max(state_dict.get(k, 0), v)
        self.write(state_dict)
        print "After flush: %s" % time.ctime()
    
    def write(self, states):
        raise NotImplementedError

    def main(self):
        with Bus() as bus:
            proxy = bus.get_service_proxy({"sixjet.provides": "states"}, multiple=True)
            proxy.watch_object("sixjet.states", self.receive_change)
            bus.wait_for_interrupt()



