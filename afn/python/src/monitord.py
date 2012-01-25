
from autobus2 import Bus
import sys
from time import sleep
from libstatmonitor import CPUMonitor, MemoryMonitor
import socket

def main():
    global status_object
    global cpu_monitor
    global memory_monitor
    if len(sys.argv) <= 1:
        host = socket.gethostname()
    else:
        host = sys.argv[1]
    with Bus() as bus:
        service = bus.create_service({"type": "monitord", "name": host})
        status_object = service.create_object("status", None, doc=
                "Provides system status information such as current CPU "
                "usage, memory usage, etc, about this machine.")
        service.activate()
        cpu_monitor = CPUMonitor()
        cpu_monitor.refresh()
        memory_monitor = MemoryMonitor()
        sleep(0.2)
        update_status()
        try:
            while True:
                sleep(5)
                update_status()
        except KeyboardInterrupt:
            print "Interrupted, shutting down"


def update_status():
    cpu_monitor.refresh()
    memory_monitor.refresh()
    cpu_list = [cpu_monitor.get_condensed(cpu) for cpu 
            in range(cpu_monitor.get_processors())]
    memory_list = memory_monitor.get()
    status_object.set_value({"cpu_i": cpu_list, "cpu":
            cpu_monitor.get_condensed(), "memory": memory_list})

























