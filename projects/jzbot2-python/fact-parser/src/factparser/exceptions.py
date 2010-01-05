

class ParseException(Exception):
    def __init__(self, reason):
        self.reason = reason
        
    def __str__(self):
        return "Syntax exception while parsing factoid: " + self.reason
