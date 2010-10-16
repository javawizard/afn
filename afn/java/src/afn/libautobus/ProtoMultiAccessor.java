package afn.libautobus;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.protobuf.GeneratedMessage;
import com.google.protobuf.Descriptors.Descriptor;
import com.google.protobuf.Descriptors.FieldDescriptor;
import com.google.protobuf.GeneratedMessage.Builder;

public class ProtoMultiAccessor<M extends GeneratedMessage, B extends Builder<B>>
{
    private String prefix;
    private String specifier;
    private List<FieldDescriptor> valueFields = new ArrayList<FieldDescriptor>();
    private Map<String, FieldDescriptor> valueFieldMap =
            new HashMap<String, FieldDescriptor>();
    private FieldDescriptor specifierField;
    private Descriptor descriptor;
    
    public ProtoMultiAccessor(Descriptor descriptor, String prefix)
    {
        this.descriptor = descriptor;
        this.prefix = prefix;
        this.specifier = prefix + "_n";
        this.specifierField = descriptor.findFieldByName(specifier);
        for (FieldDescriptor field : descriptor.getFields())
        {
            if (field.getName().startsWith(prefix))
            {
                valueFields.add(field);
                valueFieldMap.put(field.getName(), field);
            }
        }
    }
    
    public GeneratedMessage get(M message)
    {
        String suffix = (String) message.getField(specifierField);
        return (GeneratedMessage) message.getField(valueFieldMap.get(prefix + suffix));
    }
    
    public GeneratedMessage getFromBuilder(B builder)
    {
        String suffix = (String) builder.getField(specifierField);
        return (GeneratedMessage) builder.getField(valueFieldMap.get(prefix + suffix));
    }
    
    public B set(B builder, GeneratedMessage value)
    {
        boolean set = false;
        for (FieldDescriptor field : valueFields)
        {
            builder.clearField(field);
            if (value.getDescriptorForType().equals(field.getMessageType()))
            {
                set = true;
                builder.setField(field, value);
                builder
                        .setField(specifierField, field.getName()
                                .substring(prefix.length()));
            }
        }
        if (!set)
            throw new RuntimeException(
                    "Supplied instance is not of a valid type. It's type is "
                        + value.getClass().getName());
        return builder;
    }
}
