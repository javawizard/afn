
from abc import ABCMeta, abstractmethod

class ServiceProvider(object):
    """
    An abstract class specifying the interface that services must expose in
    order for Autobus to be able to publish them on the network.
    """
    __metaclass__ = ABCMeta
    
    @abstractmethod
    def __autobus_call__(self, name, args):
        """
        Calls the function with the specified name using the specified args,
        which will be some sort of iterable (usually a list). This method
        should return the return value of the function in question.
        
        This method may be called with names of functions that do not exist;
        an autobus2.exceptions.NoSuchFunctionException should be raised in such
        a case.
        """
    
    def __autobus_policy__(self, name):
        """
        Requests the calling policy that should be used for the specified
        function. This is one of the constants SYNC, THREAD, or ASYNC defined
        in autobus2.constants. If the function does not exist, any of these may
        be returned; SYNC is the usual one to return in such a case, but THREAD
        may also be used. Using ASYNC in such a case will suppress the "That
        function does not exist" error that would otherwise result.
        
        The default implementation returns THREAD in all cases.
        
        Note that I might do away with the notion of calling policies at some
        point, and just have THREAD be the default policy.
        """
    
    @abstractmethod
    def __autobus_listen__(self, listener):
        """
        Adds a new listener to this ServiceProvider. Listeners will be
        explained in more detail later, but have a look at
        technotes/autobus2-restructure.txt for more information.
        """
    
    @abstractmethod
    def __autobus_unlisten__(self, listener):
        """
        Removes a listener previously added with __autobus_listen__.
        """
