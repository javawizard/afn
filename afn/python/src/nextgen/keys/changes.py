
from collections import namedtuple

KeyAdded = namedtuple("KeyAdded", ["key", "new"])
KeyUpdated = namedtuple("KeyUpdated", ["key", "old", "new"])
KeyRemoved = namedtuple("KeyRemoved", ["key", "old"])

ItemInserted = namedtuple("ItemInserted", ["index", "new"])
ItemUpdated = namedtuple("ItemUpdated", ["index", "old", "new"])
ItemRemoved = namedtuple("ItemRemoved", ["index", "old"])
ItemRelocated = namedtuple("ItemRelocated", ["source", "target", "value"])
ItemsSwapped = namedtuple("ItemsSwapped", ["i", "j", "first", "second"])