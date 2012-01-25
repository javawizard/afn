
import gtk
import gtk.gdk
import gobject
import time
from threading import Thread
from functools import partial
from autobus2 import Bus
from afn.utils import Suppress

online_monitors = {}

def main():
    gobject.threads_init()
    with Bus() as bus:
        with bus.get_service_proxy({"type": "monitord"}, multiple=True) as proxy:
            w = gtk.Window()
            w.set_title("GMonitorOnline")
            vbox = gtk.VBox()
            w.add(vbox)
            vbox.show()
            label = gtk.Label()
            label.set_text("CPU usage:")
            vbox.add(label)
            label.show()
            w.show()
            def listener(connection, info, value):
                service_id = connection.service_id
                if value is None:
                    if service_id in online_monitors:
                        online_monitors[service_id].destroy()
                        del online_monitors[service_id]
                else:
                    if service_id not in online_monitors:
                        online_monitors[service_id] = gtk.ProgressBar()
                        online_monitors[service_id].set_text(info.get("hostname", "unknown"))
                        online_monitors[service_id].set_fraction(0)
                        vbox.add(online_monitors[service_id])
                        online_monitors[service_id].show()
                    # [0] is user, [1] is system
                    cpu = value["cpu"][0] + value["cpu"][1]
                    online_monitors[service_id].set_fraction(cpu / 100.0)
            proxy.watch_object("status", lambda * args: gobject.idle_add(partial(listener, *args)))
            gtk.main()






