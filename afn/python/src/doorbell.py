import sys
from traceback import print_exc
# sys.path.append("/afn/afn-python/src")

from parallel import Parallel #@UnresolvedImport
from autobus2 import Bus
from time import sleep
from afn.utils import print_args

p = Parallel()

previous = True

with Bus() as bus:
    bus.add_service_listener(print_args, initial=True)
    with bus.get_service_proxy({"type": "speak"}, multiple=True) as s: 
        while True:
            current = p.getInPaperOut()
            if current != previous:
                previous = current
                if not current: # Button was just pressed (i.e. the paperOut pin
                    # was just shorted to ground)
                    print "Doorbell was pressed"
                    try:
                        print s["say_text"]("someone is ringing the_front doorbell", callback=None)
                    except:
                        print_exc()
                    sleep(5)
            sleep(0.03)



