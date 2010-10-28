
from libautobus import AutobusConnection
from threading import Thread

server = AutobusConnection()
server.connect()

def get_multiple(index):
    print "Starting for " + str(index) + "..."
    interface = server["afntest.autobus3." + str(index)]
    for i in range(60):
        interface.say_hello("Alex")
    print "Done"
threads = []
for i in range(50):
    threads.append(Thread(target=get_multiple, args=(i%10,)))
    threads[-1].start()

print "Running"
for thread in threads:
    thread.join()
print "Shutting down"
server.shutdown()
