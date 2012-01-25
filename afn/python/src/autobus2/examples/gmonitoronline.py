
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
    global label
    gobject.threads_init()
    with Bus() as bus:
        with bus.get_service_proxy({"type": "monitord"}, multiple=True) as proxy:
            w = gtk.Window()
            w.set_title("GMonitorOnline")
            label = gtk.Label()
            refresh_text()
            w.add(label)
            label.show()
            w.show()
            def listener(connection, info, value):
                if value is None:
                    with Suppress(KeyError):
                        del online_monitors[connection.service_id]
                else:
                    online_monitors[connection.service_id] = info.get("hostname", "unknown")
                refresh_text()
            proxy.watch_object("cpu", listener)
            gtk.main()

def refresh_text():
    if len(online_monitors) == 0:
        label.set_text("No monitors are currently online.")
    else:
        label.set_text("The following monitors are currently online:\n\n"
                + "\n".join(online_monitors.values()))




