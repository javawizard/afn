package org.opengroove.jzbot.fact.functions;

import org.opengroove.jzbot.fact.ArgumentList;
import org.opengroove.jzbot.fact.FactContext;
import org.opengroove.jzbot.fact.Function;

public class CharFunction extends Function
{
    
    @Override
    public String evaluate(ArgumentList arguments, FactContext context)
    {
        return "" + ((char) Integer.parseInt(arguments.get(0)));
    }
    
    @Override
    public String getHelp(String topic)
    {
        return "Syntax: {{char||<number>}} -- Evaluates to a single character, which is the "
                + "ASCII character denoted by the base-10 number <number>. For example, "
                + "{{char||32}} results in \" \", {{char||49}} results in \"1\", and "
                + "{{char||65}} results in \"A\".";
    }
    
}
