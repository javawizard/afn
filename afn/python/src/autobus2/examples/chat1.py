from autobus2 import Bus
from autobus2.filter import NotEqualTo

class ChatService(object):
    def message_happened(self, sender, message):
        print sender + ": " + message


def main():
    name = raw_input("Enter your name: ")
    print "You've been successfully connected to the chat network."
    print "Type messages to send them."
    with Bus() as bus:
        service = bus.create_service({"autobus.example": "chat1"}, from_py_object=ChatService())
        with bus.get_service_proxy({"autobus.example": "chat1",
                "service": NotEqualTo(service.id)}, multiple=True) as proxy:
            while True:
                message = raw_input()
                proxy["message_happened"](name, message, callback=None)
            