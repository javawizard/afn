
from collections import namedtuple

Answer = namedtuple("Answer", ["name", "type", "ttl", "items"])
A = namedtuple("A", ["ip"])
CNAME = namedtuple("CNAME", ["target"])