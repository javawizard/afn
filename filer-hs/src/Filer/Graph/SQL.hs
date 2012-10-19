
import Filer.Graph.Interface
import Database.HDBC

data SQLiteDB = SQLiteDB Connection

instance ReadDB SQLiteDB where
    ...

instance QueryDB SQLiteDB where
    ...

instance WriteDB SQLiteDB where
    ...

instance DeleteDB SQLiteDB where
    ...