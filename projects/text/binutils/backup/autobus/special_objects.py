
import autobus
import special_interface

def interface_count():
    """
    An integer representing the number of interfaces currently registered to
    the Autobus server.
    """
    return len(autobus.interface_map)

def interfaces():
    """
    A list of maps representing all of the interfaces currently registered.
    This object's value is the value that is returned from list_interfaces().
    """
    return special_interface.interface.list_interfaces()

def interface_items():
    """
    A list of maps representing all of the interfaces currently registered,
    along with their functions. This is the same as the interfaces object,
    except that each map representing an interface has three additional keys,
    functions, events, and objects, whose values are the return values of the
    corresponding list_* functions for that particular interface.
    """
    interfaces = special_interface.interface.list_interfaces()
    for interface in interfaces:
        interface["functions"] = special_interface.interface.list_functions(interface["name"])
        interface["events"] = special_interface.interface.list_events(interface["name"])
        interface["objects"] = special_interface.interface.list_objects(interface["name"])
    return interfaces
        











