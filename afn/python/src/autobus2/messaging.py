

from concurrent import AtomicInteger

id_sequence = AtomicInteger(1)
import time


def create_id():
    return str(time.time()) + "-" + str(id_sequence.get_and_add())


def create_command(command, send_as_notice=False, **kwargs):
    result = {"_id": create_id(), "_type": 3 if send_as_notice else 1,
            "_command": command}
    result.update(kwargs)
    return result


def create_response(command_or_id, **kwargs):
    result = {"_id": command_or_id if isinstance(command_or_id, basestring)
              else command_or_id["_id"], "_type": 2}
    result.update(kwargs)
    return result
