
from librtk.protocols import ThreadedProtocol
from librtkclient import Connection
import librtkinter
from librtk.constants import DEFAULT_PORT
from socket import socket as Socket, error as SocketError
import Tkinter as tkinter
import sys
from urlparse import urlparse
from Queue import Queue, Empty
from traceback import print_exc
from utils import print_exceptions
from functools import partial

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
    socket = Socket()
    try:
        socket.connect((host, port))
    except SocketError, e:
        print "Error while connecting: " + str(e)
        return
    protocol = ThreadedProtocol(socket)
    connection, tk = librtkinter.start_connection(protocol)
    print "Started up!"
    try:
        tk.mainloop()
    except KeyboardInterrupt:
        print "Interrupted, shutting down"
        connection.close()
    print "Terminated."
    
























