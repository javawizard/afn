
import gtk
import gtk.gdk
import librtkclient
import gobject
from urlparse import urlparse
import sys
from librtk.constants import DEFAULT_PORT
from socket import socket as Socket, error as SocketError
from librtk.protocols import ThreadedProtocol
from Queue import Queue, Empty
from traceback import print_exc

# gobject.timeout_add(interval, function, ...)

gobject.threads_init()

class Widget(librtkclient.Widget):
    def destroy(self):
        self.widget.destroy()


class Window(Widget):
    def setup(self):
        self.widget = gtk.Window()
        if "title" in self.widget_properties:
            self.widget.set_title(self.widget_properties["title"])
        self.widget.show()
    
    def setup_child(self, child):
        self.widget.add(child.widget)


class VBox(Widget):
    def setup(self):
        self.widget = gtk.VBox(spacing=1)
        self.widget.show()
    
    def post_setup(self, child):
        self.widget.add(child.widget)


class Label(Widget):
    def setup(self):
        self.widget = gtk.Label(self.widget_properties["text"])
        self.widget.show()


widget_list = [Window, Label, VBox]
widget_set = dict([(w.__name__, w) for w in widget_list])
feature_set = ["widget:" + w for w in widget_set.keys()]


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
    event_queue = Queue()
    def close():
        gtk.main_quit()
    connection = librtkclient.Connection(protocol, event_queue.put, close,
            feature_set, widget_set)
    connection.start()
    def idle():
        try:
            while True:
                event_queue.get(block=False)()
        except Empty:
            pass
        except:
            print_exc()
    gtk.timeout_add(100, idle)
    try:
        gtk.main()
    except KeyboardInterrupt:
        print "Interrupted, shutting down"
    connection.close()
    





























