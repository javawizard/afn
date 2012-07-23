
from abc import ABCMeta, abstractmethod

class Command(object):
    __metaclass__ = ABCMeta
    
    @abstractmethod
    def update_parser(self, parser):
        """
        Updates the specified instance of afn.backports.ArgumentParser with
        arguments this command needs.
        """
    
    @abstractmethod
    def run(self, args):
        """
        Runs this command, given the arguments generated from the argument
        parser passed into update_parser.
        """
    
    def help(self):
        """
        Returns help/usage information for this command. Right now, this just
        returns type(self).__doc__; at some point, this will probably call
        update_parser to build a list of arguments this command needs, then
        generate help information from that, perhaps obtaining the initial
        header to write from __doc__. Subclasses can override this if they want
        to provide help in a different way.
        """
    