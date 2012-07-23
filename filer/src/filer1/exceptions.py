
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


