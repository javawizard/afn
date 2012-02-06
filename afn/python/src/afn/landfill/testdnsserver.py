
from socket import AF_INET, SOCK_DGRAM, socket as Socket
from dns.message import from_wire

def main():
    s = Socket(AF_INET, SOCK_DGRAM)
    s.bind(("", 53))
    while True:
        message = s.recv(1500)
        print repr(from_wire(message))
    