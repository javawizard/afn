
from libautobus import AutobusConnection
import sys
from time import sleep
import re
from libstatmonitor import CPUMonitor

def main():
    global status_object
    global cpu_monitor
    if len(sys.argv) <= 1:
        print "You need to specify a name for this monitor instance."
        sys.exit()
    bus = AutobusConnection()
    bus.add_interface("monitor." + sys.argv[1])
    status_object = bus.add_object("monitor." + sys.argv[1], "status",
            "Provides system status information such as current CPU "
            "usage, memory usage, etc, about this machine.", None)
    bus.start_connecting()
    cpu_monitor = CPUMonitor()
    cpu_monitor.refresh()
    try:
        while True:
            sleep(5)
            update_status()
    except KeyboardInterrupt:
        print "Interrupted, shutting down"
    finally:
        bus.shutdown()


cpu_user = {}
cpu_user_nice = {}
cpu_system = {}

def update_status():
    cpu_monitor.refresh()
    cpu_list = [cpu_monitor.get_condensed(cpu) for cpu 
            in range(cpu_monitor.get_processors())]
    status_object.set({"cpu_i": cpu_list, "cpu":
            cpu_monitor.get_condensed()})

























