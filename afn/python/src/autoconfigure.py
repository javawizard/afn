__doc__ = """\
Autoconfigure is a library that Python applications connecting to Autobus and
providing interfaces can use. It allows remote editing of configuration files
that a particular application uses. Typically, an application called "example"
will instruct Autoconfigure to register itself as configure.example for the
application's normal configuration file.

If the application is written to support this, changes made by remote
applications to configuration can be applied as soon as they are made. It's
really as simple as having the application register appropriate listeners on
individual configuration variables or the entire configuration system.
"""

sections_object_doc = """\

"""

options_object_doc = """\
"""

"""
Ok so, how this is going to work. Basically, we load up the configuration file
and represent it as a RawConfigParser. We load up the changes from the
specified configuration file, or create it if it doesn't exist. We then
register ourselves as an interface on the specified Autobus connection under
the specified name. All of the functions lock on a variable, including the
ones made available to the program that's using us. When a function's called
that changes data, the data's changed in the config parser, the configuration
file's written out, and all relevant listeners are called.

Functions that we need to provide:

sections()
add_section(name)
has_section(name)
options(section)
has_option(section, option)
get(section, option)
getint(section, option)
getfloat(section, option)
getboolean(section, option)
items(section)
set(section, option, value)
remove_option(section, option)
remove_section(section)
"""

from ConfigParser import RawConfigParser
from threading import RLock
from concurrent import synchronized

class _AutoconfigureInterface(object):
    """
    This is an interface provided by the Autoconfigure system. Autoconfigure
    is a Python library (although it could feasibly be converted into a
    standalone program usable from other programming languages) that lets
    remote Autobus clients edit configuration files. Each Autoconfigure
    interface registered on the Autobus server edits one configuration file.
    Convention is that a program registering the interface example will
    register its configuration file under the interface configure.example.
    
    If the program embedding Autoconfigure supports it (and most do), changes
    made via this interface will take effect immediately in the program
    without requiring a restart.
    
    The documentation for the functions on this interface is pulled directly
    from Python's RawConfigParser. If there are any Python-specific references
    in there, that's why.
    """
    def __init__(self, config):
        self.config = config
    
    def sections(self):
        return self.config.sections()
    
    def add_section(self, name):
        return self.config.add_section(name)
    
    def has_section(self, name):
        return self.config.has_section(name)
    
    def options(self, section):
        return self.config.options(section)
    
    def has_option(self, section, option):
        return self.config.has_option(section, option)
    
    def get(self, section, option):
        return self.config.get(section, option)
    
    def getint(self, section, option):
        return self.config.getint(section, option)
    
    def getfloat(self, section, option):
        return self.config.getfloat(section, option)
    
    def getboolean(self, section, option):
        return self.config.getboolean(section, option)
    
    def items(self, section):
        return self.config.items(section)
    
    def set(self, section, option, value):
        return self.config.set(section, option, value)
    
    def remove_option(self, section, option):
        return self.config.remove_option(section, option)
    
    def remove_section(self, section):
        return self.config.remove_section(section)
    
    # Now we'll copy the docs from RawConfigParser
    for function in locals().copy():
        if not function.__name__.startswith("_"):
            function.__doc__ = getattr(RawConfigParser,
                    function.__name__).__doc__
    del function

# TODO: convert this to one lock per Configuration instance at
# some point in the future
_lock = RLock()

class Configuration(object):
    def __init__(self, bus, interface_name, file_name):
        self.bus = bus
        self.interface_name = interface_name
        self.file_name = file_name
        self.config = RawConfigParser()
        self.config.read(file_name)
        bus.add_interface(interface_name, _AutoconfigureInterface(self))
        self.sections_object = bus.add_object(interface_name, "sections",
                sections_object_doc, self._compute_sections())
        self.options_object = bus.add_object(interface_name, "options",
                options_object_doc, self._compute_options())
    
    @synchronized(_lock)
    def _save(self):
        with open(self.file_name, "w") as file:
            self.config.write(file)
        self.sections_object.set(self._compute_sections())
        self.options_object.set(self._compute_options())
    
    @synchronized(_lock)
    def _compute_sections(self):
        return self.config.sections()
    
    @synchronized(_lock)
    def _compute_options(self):
        return dict((section, dict(self.config.items(section)))
                for section in self.config.sections())

    @synchronized(_lock)
    def sections(self):
        return self.config.sections()
    
    @synchronized(_lock)
    def add_section(self, name):
        self.config.add_section(name)
        self._save()
    
    @synchronized(_lock)
    def has_section(self, name):
        return self.config.has_section(name)
    
    @synchronized(_lock)
    def options(self, section):
        return self.config.options(section)
    
    @synchronized(_lock)
    def has_option(self, section, option):
        return self.config.has_option(section, option)
    
    @synchronized(_lock)
    def get(self, section, option):
        return self.config.get(section, option)
    
    @synchronized(_lock)
    def getint(self, section, option):
        return self.config.getint(section, option)
    
    @synchronized(_lock)
    def getfloat(self, section, option):
        return self.config.getfloat(section, option)
    
    @synchronized(_lock)
    def getboolean(self, section, option):
        return self.config.getboolean(section, option)
    
    @synchronized(_lock)
    def items(self, section):
        return self.config.items(section)
    
    @synchronized(_lock)
    def set(self, section, option, value):
        self.config.set(section, option, value)
        self._save()
    
    @synchronized(_lock)
    def remove_option(self, section, option):
        self.config.remove_option(section, option)
        self._save()
    
    @synchronized(_lock)
    def remove_section(self, section):
        self.config.remove_section(section)
        self._save()
        


























