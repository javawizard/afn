
from libautobus import AutobusConnection
import sys
from time import sleep
import re

def main():
    global status_object
    if len(sys.argv) <= 1:
        print "You need to specify a name for this monitor instance."
        sys.exit()
    bus = AutobusConnection()
    bus.add_interface("monitor." + sys.argv[1])
    status_object = bus.add_object("monitor." + sys.argv[1], "status", 
            "Provides system status information such as current CPU "
            "usage, memory usage, etc, about this machine.", None)
    bus.start_connecting()
    try:
        while True:
            sleep(3)
            update_status()
    except KeyboardInterrupt:
        print "Interrupted, shutting down"
    finally:
        bus.shutdown()


cpu_user = {}
cpu_user_nice = {}
cpu_system = {}

def update_status():
    current_cpu_user = {}
    current_cpu_system = {}
    cpu_list = []
    with open("/proc/stat") as proc_stat:
        for line in proc_stat:
            if re.match("cpu[0-9] .*", line):
                this_user, this_system = re.match(
                        "cpu. +([0-9]+) ([0-9]+)")


























