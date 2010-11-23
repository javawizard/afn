
import gobject
import gtk
from libautobus import AutobusConnection
from display import TimerSetDisplay

gobject.threads_init()

def main():
    bus = AutobusConnection()
    controller = bus["timer"]
    display = TimerSetDisplay("TrayTimer", controller, lambda: None)
    display.window.show()
