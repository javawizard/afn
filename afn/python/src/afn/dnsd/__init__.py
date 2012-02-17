
import socket as s
import dns.message

class Server(object):
    def __init__(self, host="0.0.0.0", port=53, socket=None):
        if socket:
            self.socket = socket
        else:
            self.socket = s.socket(s.AF_INET, s.SOCK_DGRAM)
            self.socket.bind((host, port))
    
    def run(self):
        while True:
            message_content = self.socket.recv(8192)
            message = dns.message.from_wire(message_content)
            print message

