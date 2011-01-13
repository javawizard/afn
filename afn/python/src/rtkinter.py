
from librtk import ThreadedProtocol
from librtkclient import Connection
from librtkinter import widget_set, feature_set
from librtk.constants import DEFAULT_PORT
from socket import socket as Socket
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
    socket.connect((host, port))
    protocol = ThreadedProtocol(socket)
    connection = Connection(protocol, tk.after_idle, tk.destroy, feature_set,
            widget_set)
    connection.tk_master = tk
    connection.start()
    print "Started up!"
    try:
        tk.mainloop()
    except KeyboardInterrupt:
        print "Interrupted, shutting down"
        connection.close()
    print "Terminated."
    
























