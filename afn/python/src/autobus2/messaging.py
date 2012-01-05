

from concurrent import AtomicInteger

id_sequence = AtomicInteger(1)
import time
import socket
import random


def create_id():
    return str(time.time()) + "-" + str(id_sequence.get_and_add())


def create_service_id():
    return create_id() + "-" + socket.gethostname() + "-" + str(random.random())


def create_command(command, send_as_notice=False, **kwargs):
    result = {"_id": create_id(), "_type": 3 if send_as_notice else 1,
            "_command": command}
    result.update(kwargs)
    return result


def create_response(command_or_id, **kwargs):
    if isinstance(command_or_id, dict) and command_or_id.get("_type") == 3:
        # This is a notice, and notices aren't ever supposed to be responded to
        return None
    result = {"_id": command_or_id if isinstance(command_or_id, basestring)
              else command_or_id["_id"], "_type": 2}
    result.update(kwargs)
    return result


def create_error(command_or_id, reason):
    return create_response(command_or_id, _error={"text": reason})


def convert_to_notice(command):
    command["_type"] = 3
    return command







