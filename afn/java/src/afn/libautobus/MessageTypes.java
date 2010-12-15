package afn.libautobus;

public class MessageTypes
{
    /*
     * The reason these are all camel-case instead of uppper case with underscores is
     * because this is how they are in the Python version of libautobus and I'm too lazy
     * to convert the case when I sync up this file from libautobus/message_types.py
     */
    public static final String RegisterInterfaceCommand = "register_interface_command";
    public static final String RegisterInterfaceResponse = "register_interface_response";
    public static final String RegisterFunctionCommand = "register_function_command";
    public static final String RegisterFunctionResponse = "register_function_response";
    public static final String CallFunctionCommand = "call_function_command";
    public static final String CallFunctionResponse = "call_function_response";
    public static final String RunFunctionCommand = "run_function_command";
    public static final String RunFunctionResponse = "run_function_response";
    public static final String ErrorResponse = "error_response";
    public static final String RegisterEventCommand = "register_event_command";
    public static final String RegisterEventResponse = "register_event_response";
    public static final String RegisterListenerCommand = "register_listener_command";
    public static final String RegisterListenerResponse = "register_listener_response";
    public static final String DeregisterListenerCommand = "deregister_listener_command";
    public static final String DeregisterListenerResponse = "deregister_listener_response";
    public static final String FireEventCommand = "fire_event_command";
    public static final String RegisterObjectCommand = "register_object_command";
    public static final String RegisterObjectResponse = "register_object_response";
    public static final String WatchObjectCommand = "watch_object_command";
    public static final String WatchObjectResponse = "watch_object_response";
    public static final String UnwatchObjectCommand = "unwatch_object_command";
    public static final String UnwatchObjectResponse = "unwatch_object_response";
    public static final String SetObjectCommand = "set_object_command";
    public static final String GetObjectValueCommand = "get_object_value_command";
    public static final String GetObjectValueResponse = "get_object_value_response";
    public static final String PingCommand = "ping_command";
    public static final String PingResponse = "ping_response";
    public static final String ActivateHackCommand = "activate_hack_command";
    public static final String ActivateHackResponse = "activate_hack_response";
    
}
