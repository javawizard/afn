
from libautobus import AutobusConnection
from threading import Thread

server = AutobusConnection()
server.connect()

interface = server["example"]

def get_multiple():
    print "Starting..."
    for i in range(30):
        interface.say_hello("Alex")
    print "Done"
threads = []
for i in range(10):
    threads.append(Thread(target=get_multiple))
    threads[-1].start()

for thread in threads:
    thread.join()
print "Shutting down"
server.shutdown()
