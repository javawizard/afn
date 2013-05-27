
from autobus import Bus, wait_for_interrupt
from autobus.providers import PyServiceProvider, PyEvent, PyObject




class FixedService(PyServiceProvider):
    sixjet_states = PyObject("sixjet_states")
    
    def __init__(self):
        self.