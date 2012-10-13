
import Filer.Graph.Interface

data SQLiteDB = SQLiteDB Connection

instance ReadDB SQLiteDB where
    ...

instance QueryDB SQLiteDB where
    ...

instance WriteDB SQLiteDB where
    ...

instance DeleteDB SQLiteDB where
    ...