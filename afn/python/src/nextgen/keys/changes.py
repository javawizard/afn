
from collections import namedtuple

KeyAdded = namedtuple("KeyAdded", ["object", "key", "value"])
KeyUpdated = namedtuple("KeyUpdated", ["object", "key", "old", "new"])
KeyRemoved = namedtuple("KeyRemoved", ["object", "key", "value"])

ItemInserted = namedtuple("ItemInserted", ["object", "index", "value"])
ItemUpdated = namedtuple("ItemUpdated", ["object", "index", "old", "new"])
ItemRemoved = namedtuple("ItemRemoved", ["object", "index", "value"])
ItemRelocated = namedtuple("ItemRelocated", ["object", "old", "new", "value"])