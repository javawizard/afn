package org.opengroove.jzbot.fact.functions;

import java.io.File;

import org.opengroove.jzbot.fact.ArgumentList;
import org.opengroove.jzbot.fact.FactContext;
import org.opengroove.jzbot.fact.FactoidException;
import org.opengroove.jzbot.fact.Function;

public class IsresourceFunction extends Function
{
    
    @Override
    public String evaluate(ArgumentList arguments, FactContext context)
    {
        String name = arguments.get(0);
        if (name.contains("..") || name.contains("\\") || name.contains("/"))
            throw new FactoidException("The resource name \"" + name
                    + "\" contains invalid characters.");
        File file = new File("resources", name);
        return file.exists() ? "1" : "0";
    }
    
    @Override
    public String getHelp(String topic)
    {
        return "Syntax: {{isresource||<name>}} -- Returns 1 if <name> denotes a valid "
                + "resource (IE {{getresource||<name>}} could be called without an error "
                + "occuring), or 0 if <name> does not denote a valid resource.";
    }
    
}
