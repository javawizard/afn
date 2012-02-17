
from afn.dnsd.server import Server
from afn.dnsd.data import Answer, A

def resolver(question_type, name):
    print (question_type, name)
    if question_type == "A":
        return [Answer(name, "A", 1, [A("1.2.3.4")])]

def main():
    server = Server(port=5053)
    server.resolvers.append(resolver)
    server.run()
