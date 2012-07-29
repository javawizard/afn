
from abc import ABCMeta, abstractmethod

class Backend(object):
    # TODO: In the future, allow the backend to specify how many jets there are
    # and stuff
    __metaclass__ = ABCMeta
    
    @abstractmethod
    def write(self, states):
        """
        Writes the specified states to the backend.
        """


