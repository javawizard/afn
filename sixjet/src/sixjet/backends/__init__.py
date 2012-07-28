
from abc import ABCMeta, abstractmethod

class Backend(object):
    # TODO: In the future, allow the backend to specify how many jets there are
    # and stuff
    __metaclass__ = ABCMeta
    
    @abstractmethod
    def set_state(self, number, state):
        """
        Sets the specified jet to the specified state.
        
        Some controllers may not actually change the state of the jet until the
        next call to self.flush. Several calls to set_state can thus be used to
        batch multiple jet state changes together.
        """
    
    @abstractmethod
    def flush(self):
        """
        Flushes changes made with set_state to the underlying system. This is
        provided primarily for controllers, like Alex's sixjet controller
        board, that buffer calls to set_state for performance reasons.
        
        This method should return as quickly as possible, but a small delay is
        acceptable; Alex's sixjet controller board's implementation (which is
        located in sixjet.backends.parallelport) blocks for a few milliseconds
        while writing the data out to the actual board.
        """


