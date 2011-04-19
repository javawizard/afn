
import pyparsing
from pyparsing import ParseException # Make this visible externally @UnusedImport

class EvaluationError(Exception):
    """
    Exception thrown when an error occurs while running a query.
    """
    def __init__(self, location, message, cause_info):
        self.location = location
        self.message = message
        self.cause_info = cause_info
        self.query = None
    
    def set_query(self, query):
        self.query = query
    
    def generate_location(self):
        if self.query:
            return ("line " + str(pyparsing.lineno(self.location, self.query.text)) +
                    ", col " + str(pyparsing.col(self.location, self.query.text)))
        else:
            return "position " + str(self.location)
    
    def __str__(self):
        return "Error at " + self.generate_location() + ": " + self.message
    
    def __repr__(self):
        return "<EvaluationError at " + self.generate_location() + ": " + self.message + ">"

