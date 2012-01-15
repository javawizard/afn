
from libautobus import create_message, COMMAND, RESPONSE
from libautobus import NOTIFICATION, encode_object
import autobus
from libautobus.message_types import *

def process_register_interface_command(message, sender, connection):
    name = message["name"]
    doc = message["doc"]
    print "Registering interface with name " + name
    interface = autobus.Interface(name, connection, doc)
    try:
        interface.register()
    except KeyError:
        connection.send_error(message, text="An interface with the same name "
                "is already registered.")
        return
    response = create_message(RegisterInterfaceResponse, message)
    connection.send(response)

def process_register_function_command(message, sender, connection):
    interface_name = message["interface_name"]
    function_names = message["name"]
    docs = message["doc"]
    if isinstance(function_names, basestring) and isinstance(docs, basestring):
        function_names = [function_names]
        docs = [docs]
    try:
        interface = autobus.lookup_interface(interface_name, sender)
    except autobus.NoSuchInterfaceException as e:
        connection.send_error(message, text=str(e))
        return
    try:
        for function_name, doc in zip(function_names, docs):
            interface.register_function(sender, function_name, doc, notify=False)
    except KeyError:
        connection.send_error(message, "A function with that name has already "
                "been registered on that interface.")
        autobus.autobus_interface.notify_object("interface_items")
        return
    autobus.autobus_interface.notify_object("interface_items")
    response = create_message(RegisterFunctionResponse, message)
    connection.send(response)

def process_call_function_command(message, sender, connection):
    try:
        interface = autobus.lookup_interface(message["interface_name"])
        function = interface.lookup_function(message["function"])
    except (autobus.NoSuchInterfaceException, autobus.NoSuchFunctionException) as e:
        connection.send_error(message, text=str(e))
        return
    if function.special:
        function.invoke_special(message, connection)
        return
    invoke_message = create_message(RunFunctionCommand,
            interface_name=interface.name, function=function.name,
            arguments=message["arguments"])
    interface.connection.send(invoke_message)
    print ("Sending run command to " + str(interface.connection.id) + 
            " with message id " + str(invoke_message["message_id"]) + 
            " whose response is to be forwarded with id " + 
            str(message["message_id"]))
    # FIXME: This causes a response to a function invocation sent as a notification
    # to still be sent when the function returns on the remote side. We need to
    # somehow not add the message to this list and perhaps suppress the "sporadic
    # message received" warning that would consequently be printed to stdout once
    # the remote client sends back the return value if the incoming message here
    # is a notification.
    interface.connection.pending_responses[invoke_message["message_id"]] = (
            sender, message["message_id"])

def process_run_function_response(message, sender, connection):
    response = create_message(CallFunctionResponse,
            RESPONSE, return_value=message["return_value"])
    if not message["message_id"] in connection.pending_responses:
        # Sporadic response received, just ignore it
        print ("Sporadic run response received from connection " + str(sender)
                + " for reported message id " + str(message["message_id"]) + 
                ". It will be ignored.")
        return
    response_connection_id, response_message_id = connection.pending_responses[message["message_id"]]
    del connection.pending_responses[message["message_id"]]
    response["message_id"] = response_message_id
    if response_connection_id in autobus.connection_map: # Make sure the connection
        # listening for the response is still alive
        response_connection = autobus.connection_map[response_connection_id]
        response_connection.send(response)

def process_register_event_command(message, sender, connection):
    interface_name = message["interface_name"]
    event_name = message["event_name"]
    doc = message["doc"]
    try:
        interface = autobus.lookup_interface(interface_name, sender)
    except autobus.NoSuchInterfaceException as e:
        connection.send_error(message, text=str(e))
        return
    try:
        interface.create_and_register_event(sender, event_name, doc)
    except KeyError:
        connection.send_error(message, "An event with that name has already "
                "been registered on that interface.")
        return
    response = create_message(RegisterEventResponse, message)
    connection.send(response)

def process_register_listener_command(message, sender, connection):
    interface_name = message["interface_name"]
    event_name = message["event_name"]
    autobus.register_event_listener(interface_name, event_name, sender)
    connection.listeners.append((interface_name, event_name))
    response = create_message(RegisterListenerResponse,
            message, interface_name=interface_name, event_name=event_name)
    connection.send(response)

def process_fire_event_command(message, sender, connection):
    interface_name = message["interface_name"]
    event_name = message["event_name"]
    arguments = message["arguments"]
    if arguments.__class__ is not list:
        connection.send_error(message, text="Event arguments must be "
                "specified as a list")
        return
    try:
        interface = autobus.lookup_interface(interface_name, sender)
    except autobus.NoSuchInterfaceException as e:
        connection.send_error(message, text=str(e))
        return
    if event_name not in interface.event_map:
        connection.send_error(message, "You need to register the event "
                "with Autobus before you can fire it.")
        return
    interface.event_map[event_name].notify(arguments)

def process_register_object_command(message, sender, connection):
    interface_name = message["interface_name"]
    object_name = message["object_name"]
    doc = message["doc"]
    value = message["value"]
    try:
        interface = autobus.lookup_interface(interface_name, sender)
    except autobus.NoSuchInterfaceException as e:
        connection.send_error(message, text=str(e))
        return
    try:
        interface.create_and_register_object(sender, object_name, doc, value)
    except KeyError:
        connection.send_error(message, "An object with that name has already "
                "been registered on that interface.")
        return
    response = create_message(RegisterObjectResponse, message)
    connection.send(response)

def process_watch_object_command(message, sender, connection):
    interface_name = message["interface_name"]
    object_name = message["object_name"]
    autobus.register_object_watch(interface_name, object_name, sender)
    connection.watches.append((interface_name, object_name))
    try:
        interface = autobus.lookup_interface(interface_name)
        object = interface.object_map[object_name]
        object_value = object.get()
    except (autobus.NoSuchInterfaceException, KeyError): # Object doesn't exist yet
        object_value = encode_object(None)
    response = create_message(WatchObjectResponse,
            message, interface_name=interface_name, object_name=object_name,
            value=object_value)
    connection.send(response)

def process_set_object_command(message, sender, connection):
    interface_name = message["interface_name"]
    object_name = message["object_name"]
    value = message["value"]
    try:
        interface = autobus.lookup_interface(interface_name, sender)
    except autobus.NoSuchInterfaceException as e:
        connection.send_error(message, text=str(e))
        return
    if object_name not in interface.object_map:
        connection.send_error(message, "You need to register the object "
                "with Autobus before you can set its value.")
        return
    interface.object_map[object_name].set_and_notify(value)

def process_ping_command(message, sender, connection):
    connection.send_new(PingResponse, message)
