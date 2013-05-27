
from parallel import Parallel
from autobus2 import Bus, wait_for_interrupt
from time import sleep
import time

class Sink(object):
    def __init__(self):
        self.service_dict = {}
    
    def receive_change(self, connection, info, name, old, new):
        print "Changed!"
        try:
            if new is None:
                if name in self.service_dict:
                    del self.service_dict[name]
            else:
                self.service_dict[name] = new
            self.compute_and_write()
        except BaseException as e:
            print "Exception: %r" % e
    
    def compute_and_write(self):
        print "Before flush: %s" % time.ctime()
        state_dict = {}
        for states in self.service_dict.values():
            print states
            for k, v in states.iteritems():
                state_dict[k] = max(state_dict.get(k, 0), v)
        print state_dict
        self.write(state_dict)
        print "After flush: %s" % time.ctime()
    
    def write(self, states):
        raise NotImplementedError

    def main(self):
        with Bus() as bus:
            proxy = bus.get_service_proxy({"sixjet.provides": "states"}, multiple=True)
            proxy.watch_object("sixjet_states", self.receive_change)
            wait_for_interrupt()



