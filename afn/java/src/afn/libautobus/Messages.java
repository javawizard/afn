package afn.libautobus;

import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;

import static afn.libautobus.Translation.map;

class Messages
{
    public static final int COMMAND = 1;
    public static final int RESPONSE = 2;
    public static final int NOTIFICATION = 3;
    
    private static final AtomicLong nextId = new AtomicLong();
    
    public static Map createMessage(String action)
    {
        return createMessage(action, COMMAND);
    }
    
    @SuppressWarnings("unchecked")
    public static Map createMessage(String action, Object messageType, Object... kwargs)
    {
        Map message = map(kwargs);
        message.put("action", action);
        if (messageType instanceof Map)
        {
            Map typeAsMap = (Map) messageType;
            message.put("message_id", typeAsMap.get("message_id"));
            if (((int) (Integer) typeAsMap.get("message_type")) == NOTIFICATION)
                message.put("message_type", null);
            else
                message.put("message_type", RESPONSE);
        }
        else
        {
            message.put("message_id", getNextId());
            message.put("message_type", messageType);
        }
        return message;
    }
    
    private static Object getNextId()
    {
        return nextId.incrementAndGet();
    }
}
