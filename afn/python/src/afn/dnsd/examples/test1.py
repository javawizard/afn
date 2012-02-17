
from afn.dnsd.server import Server
from dns.rrset import RRset
from dns.rdtypes.IN import *
from dns.rdataclass import from_text as text_to_class
from dns.rdatatype import from_text as text_to_type

def resolver(question_type, name):
    print (question_type, name)
    if question_type == "A":
        rrset = RRset(name, text_to_class("IN"), text_to_type("A"))
        rrset.ttl = 1
        a = A.A(text_to_class("IN"), text_to_type("A"), "1.2.3.4")
        rrset.items.append(a)
        return rrset
            

def main():
    server = Server(port=5053)
    server.resolvers.append(resolver)
    server.run()
