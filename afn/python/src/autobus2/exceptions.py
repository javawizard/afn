

class AutobusException(Exception):
    pass


class TimeoutException(AutobusException):
    pass


class ConnectionLostException(AutobusException):
    pass


