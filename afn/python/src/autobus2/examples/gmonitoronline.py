
import gtk
import gtk.gdk
import gobject
import time
from threading import Thread
from functools import partial
from autobus2 import Bus

def main():
    gobject.threads_init()
    window = gtk.Window()
    window.set_title("hello world")
    window.show()
    def recurring():
        for i in range(3):
            time.sleep(1)
            gobject.idle_add(partial(window.set_title, "number " + str(i)))
        gobject.idle_add(window.destroy)
    Thread(target=recurring).start()
    gtk.main()