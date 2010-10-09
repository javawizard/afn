
from libautobus import AutobusConnection
from threading import Thread

server = AutobusConnection()
server.connect()

def get_multiple(index):
    print "Starting for " + str(index) + "..."
    interface = server["afntest.autobus3." + str(index)]
    for i in range(30):
        interface.say_hello("Alex")
    print "Done"
threads = []
for i in range(30):
    threads.append(Thread(target=get_multiple, args=(i%3,)))
    threads[-1].start()

for thread in threads:
    thread.join()
print "Shutting down"
server.shutdown()
