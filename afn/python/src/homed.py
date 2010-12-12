
from libautobus import AutobusConnection
from autoconfigure import Configuration

class Interface(object):
    """
    The home daemon is a higher-level interface to ActiveHome. It allows
    modules to be configured, turned on, and turned off. It supports various
    types of modules, such as cameras (turning on any camera will cause all
    other cameras to be turned off) and chimes (turning off a chime has no
    effect, but it can be turned on multiple times).
    
    Homed delegates all of its control functions to activehomed. An instance
    of activehomed must be running on the same Autobus server that homed is
    attached to.
    """
    def on(self, address):
        """
        Turns the specified module on. This module must be in the configuration
        file. This function will take special module types into effect; for
        example, calling this function on a camera will automatically turn all
        other cameras off.
        """
        address = address.lower()
        verify_existing_address(address)
        # Assume powerline for now. In the future, we'll want to have some
        # configuration variable that specifies whether this is powerline
        # or rf or what.
        activehome.action.send("sendplc", address, "on")
    
    def off(self, address):
        """
        Turns the specified module off. This follows the same general contract
        as the on function.
        """
        address = address.lower()
        verify_existing_address(address)
        activehome.action.send("sendplc", address, "off")

def verify_existing_address(address):
    if len(address) != 2:
        raise Exception("Addresses have to be exactly 2 characters in length.")
    if not config.has_section(address):
        raise Exception("The specified address is not present in the "
                "configuration. If you want to send X10 commands irrespective "
                "of whether they're in configuration, check out the action "
                "function of the activehome interface.")

def main():
    global bus
    global config
    global activehome
    bus = AutobusConnection()
    activehome = bus["activehome"]
    bus.add_interface("home", Interface())
    config = Configuration(bus, "configure.home", "homed.conf")
    bus.start_connecting()
    bus.interrupt_loop()
    