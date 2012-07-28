
from sixjet.backends import Backend

default_map = """
    a         b
         g

   l   m   n   h
f                 c
   k   p   o   i

         j
    e         d
"""


class ConsoleBackend(object):
    def __init__(self, jet_count):
        self.states = [False] * jet_count





