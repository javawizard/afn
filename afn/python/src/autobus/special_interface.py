
import autobus
import special_objects

class AutobusInterface(object):
    """
    An interface built-in to the Autobus server that provides information about
    the server, such as the interfaces that are currently registered. To see a
    list of functions that are on this interface, call the function
    list_functions on this interface, passing in the string "autobus". The
    resulting list should provide plenty of information.
    """
    def list_interfaces(self):
        """
        Returns a list of all interfaces currently registered to the autobus
        server. The return value is a list of maps, with each map representing
        one interface. Each map has the following keys:
        
        owner: The numeric id of the connection that registered this interface.
        
        name: The name of the interface.
        
        special: True if this interface is special (I.E. registered by
        server-side code instead of by a client), false if it is a normal
        interface.
        
        doc: The documentation of the interface if the client supplied any such
        documentation, or the empty string if they didn't.
        """
        result = []
        for interface in autobus.interface_map.values():
            result.append({"owner": interface.connection.id if not
                    interface.special else 0, "name": interface.name,
                    "special": interface.special, "doc": interface.doc})
        return result
    
    def list_functions(self, interface_name):
        """
        Returns a list of all functions currently registered to the specified
        interface. If there's no such interface, an exception will be thrown.
        This function works correctly on special interfaces as well as normal
        interfaces.
        
        The return value is similar to the return value of list_interfaces().
        Each map within the list that's returned represents a function on the
        interface. 
        """
        try:
            interface = autobus.interface_map[interface_name]
        except KeyError:
            raise Exception("No such interface.")
        return interface.describe_functions()
    
    def list_events(self, interface_name):
        """
        Sane as list_functions, but returns a list of events available on this
        interface.
        """
        try:
            interface = autobus.interface_map[interface_name]
        except KeyError:
            raise Exception("No such interface.")
        return interface.describe_events()
        
    def list_objects(self, interface_name):
        """
        Same as list_functions, but returns a list of objects available on this
        interface. 
        """
        try:
            interface = autobus.interface_map[interface_name]
        except KeyError:
            raise Exception("No such interface.")
        return interface.describe_objects()
        
    def get_processed_message_count(self):
        """
        Returns the number of messages that Autobus has received and processed
        since it started up. This does not count messages that Autobus has
        sent; only inbound messages are counted.
        """
        return autobus.processed_message_count

interface = AutobusInterface()
