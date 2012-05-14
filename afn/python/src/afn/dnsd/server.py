"""
Resolvers have the signature resolver(source, type, name) -> answer

type is the string type of the request ("A", "MX", etc)

name is the request itself ("www.example.com" etc)

answer is a list of Answer instances
"""

import socket as s
import dns.message
import dns.rrset
from dns.rdtypes.IN import *
from dns.rdtypes.ANY import *
from dns.rdataclass import from_text as text_to_class
from dns.rdatatype import from_text as text_to_type
from afn.dnsd import data as d

class Server(object):
    def __init__(self, host="0.0.0.0", port=53, socket=None):
        if socket:
            self.socket = socket
        else:
            self.socket = s.socket(s.AF_INET, s.SOCK_DGRAM)
            self.socket.bind((host, port))
        self.resolvers = []
        self.filters = []
    
    def run(self):
        while True:
            message_content, (source_host, source_port) = self.socket.recvfrom(8192)
            print "DNSD: Message is " + repr(message_content)
            message = dns.message.from_wire(message_content)
            print "DNSD: Request: " + repr(message)
            response = self.process_message((source_host, source_port), message)
            response_content = response.to_wire()
            print "DNSD: Response: " + repr(response)
            print "DNSD: Response is " + repr(response_content)
            self.socket.sendto(response_content, (source_host, source_port))
    
    def process_message(self, source, message):
        questions = message.question
        answers = [self.process_question(source, q) for q in questions]
        answers = [answer for answer_list in answers for answer in answer_list]
        response = dns.message.make_response(message)
        response.answer += answers
        return response
    
    def process_question(self, source, question):
        if dns.rdataclass.to_text(question.rdclass) != "IN":
            raise Exception("Only IN class queries are supported.")
        datatype = dns.rdatatype.to_text(question.rdtype)
        name = question.name
        textual_name = ".".join(question.name.labels[:-1])
        answers = None
        for resolver in self.resolvers:
            answers = resolver(source, datatype, textual_name)
            if answers:
                break
        if not answers:
            raise Exception("No sources could answer " + repr(question))
        return data_to_dnspy(answers)


def data_to_dnspy(data):
    if isinstance(data, list):
        return [data_to_dnspy(x) for x in data]
    if isinstance(data, d.Answer):
        result = dns.rrset.RRset(dns.name.from_text(data.name),
                text_to_class("IN"), text_to_type(data.type))
        result.ttl = data.ttl
        result.items += data_to_dnspy(data.items)
        return result
    if isinstance(data, d.A):
        return A.A(text_to_class("IN"), text_to_type("A"), data.ip)
    if isinstance(data, d.AAAA):
        return AAAA.AAAA(text_to_class("IN"), text_to_type("AAAA"), data.ip)
    if isinstance(data, d.CNAME):
        return CNAME.CNAME(text_to_class("IN"), text_to_type("CNAME"),
                dns.name.from_text(data.target))
    raise Exception("Can't convert instance of " + repr(type(data)) + ": " + repr(data))
                






























            

