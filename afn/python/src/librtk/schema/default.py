
import xmlparser
import os

schema = xmlparser.parse_file(os.path.join(os.path.split(__file__)[0], "default.xml"))

