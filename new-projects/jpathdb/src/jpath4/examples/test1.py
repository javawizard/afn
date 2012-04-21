
from jpath4.db.database import Database
from jpath4.db.backends import zodb
from jpath4.query.interpreter import Interpreter
from jpath4.query.translate import json_to_jpath, jpath_to_json
from jpath4.fileutils import File

json = {"a": {"b": {"c": "e"}}}

storage = zodb.DBStorage(File(__file__).parent().parent().parent().parent().child("landfill").child("test1-db").path)
db = Database(storage, Interpreter)

print "start"
print db.run_query('.', False, {"x": json})
print "end"


