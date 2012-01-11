from autobus2 import Bus
from autobus2.filter import NotEqualTo
from afn.utils.concurrent import dump_thread_traces

class ChatService(object):
    def message_happened(self, sender, message):
        print sender + ": " + message

def signed_on(proxy, connection, info):
    print info["name"] + " has signed on."

def signed_off(proxy, connection, info):
    print info["name"] + " has signed off."

def main():
    name = raw_input("Enter your name: ")
    print "You've been successfully connected to the chat network."
    print "Type messages to send them."
    with Bus() as bus:
        # Publish ChatService to allow messages to be sent to us
        service = bus.create_service({"autobus.example": "chat1", "name": name},
                from_py_object=ChatService())
        # Connect to other chat services on the network so that we can send
        # messages to them
        with bus.get_service_proxy({"autobus.example": "chat1",
                "service": NotEqualTo(service.id)}, bind_function=signed_on,
                unbind_function=signed_off, multiple=True) as proxy:
            # Loop until the user hits Ctrl+C
            while True:
                try:
                    message = raw_input()
                except KeyboardInterrupt:
                    return
                if message == "==trace==":
                    dump_thread_traces()
                    continue
                proxy["message_happened"](name, message, callback=None)




