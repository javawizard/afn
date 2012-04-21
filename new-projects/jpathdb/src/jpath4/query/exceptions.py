
class JPathException(Exception):
    """
    A class representing exceptions thrown by various parts of JPath.
    """


class ImportException(JPathException):
    """
    Exception thrown when a module tries to import another module that does
    not exist or that the corresponding binder can't otherwise properly bind.
    """


class TypeException(JPathException):
    """
    Exception thrown while evaluating JPath code when a particular object is
    not of a particular required type
    """


class FunctionLookupException(JPathException):
    """
    Exception thrown when a function that doesn't exist is called.
    """


class OtherException(JPathException):
    """
    Exception thrown when no other subclass of JPathException makes sense to
    throw for whatever the error was that merited throwing an exception.
    """

