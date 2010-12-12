package afn.libautobus;

import static afn.libautobus.Translation.encodeObject;
import static afn.libautobus.Translation.decodeObject;
import static afn.libautobus.Messages.createMessage;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import static afn.libautobus.Messages.COMMAND;

public class FunctionWrapper
{
    private InterfaceWrapper parent;
    private String name;
    
    FunctionWrapper(InterfaceWrapper parent, String name)
    {
        this.parent = parent;
        this.name = name;
    }
    
    @SuppressWarnings("unchecked")
    public Object invoke(Object... args)
    {
        List instanceArgs = new ArrayList();
        for (Object arg : args)
            instanceArgs.add(encodeObject(arg));
        Map message =
                createMessage(MessageTypes.CallFunctionCommand, COMMAND, "interface_name",
                        this.parent.name, "function", this.name, "arguments", instanceArgs);
        Map response = this.parent.bus.query(message);
        if (response.get("action").equals(MessageTypes.ErrorResponse))
            throw new RuntimeException("Server-side error: "
                + (response.containsKey("text") ? response.get("text")
                        : "No additional error information was sent by the server."));
        Object result = decodeObject(response.get("return_value"));
        if (result instanceof RuntimeException)
            throw (RuntimeException) result;
        return result;
    }
    
    @SuppressWarnings("unchecked")
    public void send(Object... args)
    {
        List instanceArgs = new ArrayList();
        for (Object arg : args)
            instanceArgs.add(encodeObject(arg));
        Map message =
                createMessage(MessageTypes.CallFunctionCommand, COMMAND, "interface_name",
                        this.parent.name, "function", this.name, "arguments", instanceArgs);
        this.parent.bus.send(message);
    }
}
