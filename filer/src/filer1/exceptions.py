
from afn.utils.exceptions import SemanticException

class FilerError(SemanticException):
    pass


class InternalError(FilerError):
    pass


class InvalidType(InternalError):
    _format = "Invalid type %(type)r (must be 'file' or 'folder')"


class MissingFileInfo(InternalError):
    _format = "Missing file update info in revision data %(data)r"
    
    def __init__(self, data):
        pass


class BECError(SemanticException):
    pass


class KeyType(BECError):
    _format = "Keys must be strings, but the key %(key)r is not"


class ValueType(BECError):
    _format = "Values must be a JSONable type, but the value %(value)r is not"


class LargeValue(BECError):
    _format = ("The value %r can't be retrieved as a string because it's "
            "rather large. Try reading it as a stream with get_stream instead.")


class InvalidTypeCode(BECError):
    _format = 'The type code "%(type)s" is not a valid type code.'


