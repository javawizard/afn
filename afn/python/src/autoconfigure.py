__doc__ = """\
FIXME: THIS HAS BEEN PORTED TO AUTOBUS 2 BUT NOT YET TESTED. IT NEEDS TO BE
TESTED AND ANY BUGS WORKED OUT BEFORE IT CAN BE USED AGAIN.

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
register ourselves as a service on the specified Autobus bus under
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

class _AutoconfigureService(object):
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
    for name, function in locals().items()[:]:
        if not name.startswith("_"):
            function.__doc__ = getattr(RawConfigParser, name).__doc__
    del name
    del function

# TODO: convert this to one lock per Configuration instance at
# some point in the future
_lock = RLock()

class Configuration(object):
    def __init__(self, bus, target_name, file_name):
        self.bus = bus
        self.target_name = target_name
        self.file_name = file_name
        self.config = RawConfigParser()
        self.config.read(file_name)
        if target_name is not None:
            self.service = bus.create_service({"type": "autoconfigure", "target": target_name},
                    active=False, use_py_object=_AutoconfigureService(self))
            self.sections_object = self.service.create_object("sections",
                    self._compute_sections(), doc=sections_object_doc)
            self.options_object = self.service.create_object("options",
                    self._compute_options(), doc=options_object_doc)
    
    @synchronized(_lock)
    def _save(self):
        with open(self.file_name, "w") as file:
            self.config.write(file)
        self.sections_object.set_value(self._compute_sections())
        self.options_object.set_value(self._compute_options())
    
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
        


























