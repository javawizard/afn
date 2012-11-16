
from threading import local as Local

ERROR = 5
WARNING = 4
INFO = 3
DEBUG = 2
ALL = 0

active_level = INFO
_stack_local = Local()


class TreeLogger(object):
    def __init__(self, parent, message, level):
        self._parent = parent
        if parent is None:
            self._indent = 0
        else:
            self._indent = parent._indent + 1
        self._message = message
        self._level = level
        self._displayed = False
        self.maybe_display()
    
    def display(self):
        if not self._displayed:
            self._displayed = True
            if self._parent:
                self._parent.display()
            print ("  " * self._indent) + self._message
    
    def maybe_display(self):
        if self._level >= active_level:
            self.display()
    
    def log(self, message, level=INFO):
        return TreeLogger(self, message, level)
    
    def error(self, message):
        return self.log(message, ERROR)
    
    def warning(self, message):
        return self.log(message, WARNING)
    
    def info(self, message):
        return self.log(message, INFO)
    
    def debug(self, message):
        return self.log(message, DEBUG)
    
    def __enter__(self):
        _get_stack().append(self)
    
    def __exit__(self, *args):
        _get_stack().pop()


_root = TreeLogger(None, "Root logger (Message TBD)", INFO)


def _get_stack():
    return _stack_local.__dict__.setdefault("stack", [_root])


def _get_current():
    return _get_stack()[-1]


def log(message, level=INFO):
    return _get_current().log(message, level)

def error(message):
    return _get_current().error(message)

def warning(message):
    return _get_current().warning(message)

def info(message):
    return _get_current().info(message)

def debug(message):
    return _get_current().debug(message)




