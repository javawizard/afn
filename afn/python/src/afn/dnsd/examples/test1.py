
from afn.dnsd.server import Server
from afn.dnsd.data import Answer, A, CNAME

def resolver(source, question, name):
    print (source, question, name)
    print "Question: ", source, question, name
    if question == "A":
        return [
            Answer(name, "CNAME", 1, [
                CNAME("www.redirected.example.com")
            ]),
            Answer("www.redirected.example.com", "A", 1, [
                A("1.2.3.4"),
                A("5.6.7.8"),
                A("9.10.11.12")
            ])
        ]

def main():
    server = Server(port=5053)
    server.resolvers.append(resolver)
    server.run()
