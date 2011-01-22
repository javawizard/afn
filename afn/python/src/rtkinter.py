
from librtk.protocols import ThreadedProtocol
from librtkclient import Connection
from librtkinter import widget_set, feature_set
from librtk.constants import DEFAULT_PORT
from socket import socket as Socket, error as SocketError
import Tkinter as tkinter
import sys
from urlparse import urlparse


def main():
    if len(sys.argv) <= 1:
        print "You need to specify the url of the application to display."
        return
    scheme, location = urlparse(sys.argv[1])[0:2]
    if scheme != "rtk":
        print ("Only rtk:// urls are supported for now. I might add an HTTP "
                "transport at some point in the future.")
    if ":" not in location:
        location += ":" + str(DEFAULT_PORT)
    host, port = location.split(":")
    port = int(port)
    tk = tkinter.Tk()
    tk.withdraw()
    socket = Socket()
    try:
        socket.connect((host, port))
    except SocketError, e:
        print "Error while connecting: " + str(e)
        return
    protocol = ThreadedProtocol(socket)
    connection = Connection(protocol, tk.after_idle, tk.destroy, feature_set,
            widget_set)
    connection.tk_master = tk
    connection.start()
    def idle(): # This causes the tkinter main thread to return into Python
        # code at least once every second, which allows us to properly respond
        # to KeyboardInterrupts. Without this, a KeyboardInterrupt would not
        # be received until a tkinter event was fired after the interrupt was
        # triggered.
        tk.after(1000, idle)
    tk.after_idle(idle)
    print "Started up!"
    try:
        tk.mainloop()
    except KeyboardInterrupt:
        print "Interrupted, shutting down"
        connection.close()
    print "Terminated."
    
























