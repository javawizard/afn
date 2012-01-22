import sys
from traceback import print_exc
# sys.path.append("/afn/afn-python/src")

from parallel import Parallel #@UnresolvedImport
from autobus2 import Bus
from time import sleep
from afn.utils import print_args
from functools import partial

def run():
    p = Parallel()
    previous = True
    with Bus() as bus:
        bus.add_service_listener(partial(print_args, "SERVICE LISTENER"), initial=True)
        with bus.get_service_proxy({"type": "speak"}, bind_function=partial(print_args, "BIND"),
                unbind_function=partial(print_args, "UNBIND"), multiple=True) as s: 
            while True:
                current = p.getInPaperOut()
                if current != previous:
                    previous = current
                    if not current: # Button was just pressed (i.e. the paperOut pin
                        # was just shorted to ground)
                        print "Doorbell was pressed"
                        try:
                            print s["say_text"]("someone is ringing the_front doorbell", callback=partial(print_args, "CALLBACK"))
                        except:
                            print_exc()
                        sleep(5)
                sleep(0.03)



