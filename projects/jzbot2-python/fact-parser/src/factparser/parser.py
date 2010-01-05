from entities import *
from sinks import *

def parse():
    pass

class FactContext:
    globalVars = {}
    def __init__(self):
        self.localVars = {}
        self.subroutines = {}
        self.action = False

