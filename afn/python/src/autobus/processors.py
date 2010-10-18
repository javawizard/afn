
from libautobus import MessageValue, create_message_pair, COMMAND, RESPONSE
from libautobus import NOTIFICATION, encode_object
import autobus
from autobus_protobuf import autobus_pb2 as protobuf

def process_register_interface_command(message, sender, connection):
    command = MessageValue[message]
    name = command.name
    doc = command.doc
    print "Registering interface with name " + name
    interface = autobus.Interface(name, connection, doc)
    try:
        interface.register()
    except KeyError:
        connection.send_error(message, "An interface with the same name "
                "is already registered.")
        return
    response_message, response = create_message_pair(protobuf.RegisterInterfaceResponse, message)
    connection.send(response_message)

def process_register_function_command(message, sender, connection):
    command = MessageValue[message]
    interface_name = command.interface_name
    function_names = command.name
    docs = command.doc
    try:
        interface = autobus.lookup_interface(interface_name, sender)
    except autobus.NoSuchInterfaceException as e:
        connection.send_error(message, text=str(e))
        return
    try:
        for function_name, doc in zip(function_names, docs):
            print "Registering, name is " + function_name
            interface.register_function(sender, function_name, doc, notify=False)
    except KeyError:
        connection.send_error(message, "A function with that name has already "
                "been registered on that interface.")
        autobus.autobus_interface.notify_object("interface_items")
        return
    autobus.autobus_interface.notify_object("interface_items")
    response_message, response = create_message_pair(protobuf.RegisterFunctionResponse, message)
    connection.send(response_message)

def process_call_function_command(message, sender, connection):
    command = MessageValue[message]
    try:
        interface = autobus.lookup_interface(command.interface_name)
        function = interface.lookup_function(command.function)
    except (autobus.NoSuchInterfaceException, autobus.NoSuchFunctionException) as e:
        connection.send_error(message, text=str(e))
        return
    if function.special:
        function.invoke_special(message, command, connection)
        return
    invoke_message, invoke_command = create_message_pair(protobuf.RunFunctionCommand,
            interface_name=interface.name, function=function.name,
            arguments=command.arguments)
    interface.connection.send(invoke_message)
    print ("Sending run command to " + str(interface.connection.id) + 
            " with message id " + str(invoke_message.message_id) + 
            " whose response is to be forwarded with id " + 
            str(message.message_id))
    interface.connection.pending_responses[invoke_message.message_id] = (
            sender, message.message_id)

def process_run_function_response(message, sender, connection):
    result = MessageValue[message]
    response, call_response = create_message_pair(protobuf.CallFunctionResponse,
            RESPONSE, return_value=result.return_value)
    if not message.message_id in connection.pending_responses:
        # Sporadic response received, just ignore it
        print ("Sporadic run response received from connection " + str(sender)
                + " for reported message id " + str(message.message_id) + 
                ". It will be ignored.")
        return
    response_connection_id, response_message_id = connection.pending_responses[message.message_id]
    del connection.pending_responses[message.message_id]
    response.message_id = response_message_id
    if response_connection_id in autobus.connection_map: # Make sure the connection
        # listening for the response is still alive
        response_connection = autobus.connection_map[response_connection_id]
        response_connection.send(response)

def process_register_object_command(message, sender, connection):
    command = MessageValue[message]
    interface_name = command.interface_name
    object_name = command.object_name
    doc = command.doc
    value = command.value
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
    response_message, response = create_message_pair(protobuf.RegisterObjectResponse, message)
    connection.send(response_message)

def process_watch_object_command(message, sender, connection):
    command = MessageValue[message]
    interface_name = command.interface_name
    object_name = command.object_name
    autobus.register_object_watch(interface_name, object_name, sender)
    connection.watches.append((interface_name, object_name))
    try:
        interface = autobus.lookup_interface(interface_name)
        object = interface.object_map[object_name]
        object_value = object.get()
    except (autobus.NoSuchInterfaceException, KeyError): # Object doesn't exist yet
        object_value = encode_object(None)
    response_message, response = create_message_pair(protobuf.WatchObjectResponse,
            message, interface_name=interface_name, object_name=object_name,
            value=object_value)
    connection.send(response_message)

def process_set_object_command(message, sender, connection):
    command = MessageValue[message]
    interface_name = command.interface_name
    object_name = command.object_name
    value = command.value
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
