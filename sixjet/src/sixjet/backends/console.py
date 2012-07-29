
from sixjet.backends import Backend
import time

default_map = """
     a         b
          g

    l   m   n   h
f                   c
    k   p   o   i

          j
     e         d
"""


class ConsoleBackend(object):
    def __init__(self, map=default_map, on="^", off="."):
        self.map = map
        self.on = on
        self.off = off
    
    def write(self, states):
        text = self.map
        for i, state in enumerate(states):
            text = text.replace("abcdefghijklmnop"[i],
                    self.on if state else self.off)
        print time.ctime()
        print text





