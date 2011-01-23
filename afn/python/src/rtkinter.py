
from librtk.protocols import ThreadedProtocol
from librtkclient import Connection
from librtkinter import widget_set, feature_set
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
    tk = tkinter.Tk()
    tk.withdraw()
    socket = Socket()
    try:
        socket.connect((host, port))
    except SocketError, e:
        print "Error while connecting: " + str(e)
        return
    protocol = ThreadedProtocol(socket)
    event_queue = Queue()
    connection = Connection(protocol, event_queue.put, tk.destroy, feature_set,
            widget_set)
    connection.tk_master = tk
    connection.start()
    def idle():
        try:
            for event in iter(partial(event_queue.get, block=False), None):
                with print_exceptions:
                    event()
            # If we get here, we didn't throw Empty while iterating but we got
            # a None, so we're supposed to drop out of the loop
            return
        except Empty:
            pass
        except:
            print_exc()
        tk.after(200, idle)
    tk.after(200, idle)
    print "Started up!"
    try:
        tk.mainloop()
    except KeyboardInterrupt:
        print "Interrupted, shutting down"
        event_queue.put(None)
        connection.close()
    print "Terminated."
    
























