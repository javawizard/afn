
import gobject
import gtk
from libautobus import AutobusConnection

gobject.threads_init()


def main():
    global bus
    global timerd
    bus = AutobusConnection()
    timerd = bus["timer"]