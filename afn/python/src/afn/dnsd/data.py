
from collections import namedtuple

Answer = namedtuple("Answer", ["name", "type", "ttl", "items"])
A = namedtuple("A", ["ip"])
AAAA = namedtuple("AAAA", ["ip"])
CNAME = namedtuple("CNAME", ["target"])
MX = namedtuple("MX", ["ip"])