
import code
import socket
import threading

class Console(code.InteractiveConsole):
    def __init__(self, s, local_vars=None):
        code.InteractiveConsole.__init__(self, local_vars)
        self.socket = s
        self.connection = s.makefile()
    
    def write(self, text):
        if self.connection:
            self.connection.write(text)
            self.connection.flush()
    
    def raw_input(self, prompt=""):
        self.write(prompt)
        try:
            text = self.connection.readline()
            if not text:
                raise Exception("Remote end closed the connection")
            return text
        except BaseException as e:
            print "Telnet console session disconnected: %s" % e
            self.connection.close()
            self.connection = None
            self.socket.close()
            raise EOFError()


def listen(port, local_vars=None):
    t = threading.Thread(target=lambda: listen_synchronously(port, local_vars))
    t.setDaemon(True)
    t.start()


def listen_synchronously(port, local_vars=None):
    server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server.bind(("127.0.0.1", port))
    server.listen(10)
    while True:
        s, _ = server.accept()
        console = Console(s, local_vars)
        threading.Thread(target=console.interact).start()












