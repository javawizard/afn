package afn.libautobus;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import afn.libautobus.protocol.Protobuf.GeneratedMessage;

public class ProtoMultiAccessor<M extends GeneratedMessage<?>>
{
    private String prefix;
    private String specifier;
    private List<String> valueFields = new ArrayList<String>();
    
    @SuppressWarnings("unchecked")
    public ProtoMultiAccessor(Class<M> messageClass, String prefix)
    {
        this.prefix = prefix;
        this.specifier = prefix + "N";
        for (String fieldName : (Set<String>) ProtocolUtils.invokeStatic(messageClass,
                "getFieldNames"))
            if (fieldName.startsWith(prefix))
                valueFields.add(fieldName);
    }
    
    public <T extends GeneratedMessage<?>> T get(M message)
    {
        String suffix = message.get(specifier);
        return (T) message.get(prefix + suffix);
    }
    
    public void set(M target, GeneratedMessage<?> value)
    {
        boolean set = false;
        for (String field : valueFields)
        {
            target.set(field, null);
            if (target.getInstanceClassForField(field).isInstance(value))
            {
                set = true;
                target.set(field, value);
                target.set(specifier, field.substring(prefix.length()));
            }
        }
        if (!set)
            throw new RuntimeException(
                    "Supplied instance is not of a valid type. It's type is "
                        + value.getClass().getName());
    }
}
