
from collections import namedtuple

KeyAdded = namedtuple("KeyAdded", ["object", "key", "new"])
KeyUpdated = namedtuple("KeyUpdated", ["object", "key", "old", "new"])
KeyRemoved = namedtuple("KeyRemoved", ["object", "key", "old"])

ItemInserted = namedtuple("ItemInserted", ["object", "index", "new"])
ItemUpdated = namedtuple("ItemUpdated", ["object", "index", "old", "new"])
ItemRemoved = namedtuple("ItemRemoved", ["object", "index", "old"])
ItemRelocated = namedtuple("ItemRelocated", ["object", "source", "target", "value"])
ItemsSwapped = namedtuple("ItemsSwapped", ["object", "i", "j", "first", "second"])