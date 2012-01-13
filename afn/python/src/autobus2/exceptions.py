

class AutobusException(Exception):
    pass


class TimeoutException(AutobusException):
    pass


class ConnectionException(AutobusException):
    pass


class ConnectionLostException(ConnectionException):
    pass


class NotConnectedException(ConnectionException):
    pass


class InvalidValueException(AutobusException):
    pass


class RemoteUserException(AutobusException):
    pass


class CommandErrorException(AutobusException):
    pass


class NullMessageException(AutobusException):
    pass


class QueryCanceledException(AutobusException):
    pass


class NoMatchingServiceException(AutobusException):
    pass


class ClosedException(AutobusException):
    pass


