
from collections import namedtuple

ValueSet = namedtuple("ValueSet", ["value"])

class ValueSender(object):
    def add_receiver(self, receiver):
        pass
    
    def remove_receiver(self, receiver):
        pass


class ValueReceiver(object):
    def receive(self, action):
        pass
    
    def validate(self, action):
        pass


class BindCell(ValueSender, ValueReceiver):
    def __init__(self, value):
        self._value = value
        self._receivers = []
    
    def receive(self, action):
        

