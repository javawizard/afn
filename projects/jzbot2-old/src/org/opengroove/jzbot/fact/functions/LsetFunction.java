package org.opengroove.jzbot.fact.functions;

import org.opengroove.jzbot.fact.ArgumentList;
import org.opengroove.jzbot.fact.FactContext;
import org.opengroove.jzbot.fact.Function;

public class LsetFunction extends Function
{
    
    @Override
    public String evaluate(ArgumentList arguments, FactContext context)
    {
        context.getLocalVars().put(arguments.get(0), arguments.get(1));
        return "";
    }
    
    @Override
    public String getName()
    {
        return "lset";
    }
    
    @Override
    public String getHelp(String topic)
    {
        return "Syntax: {{lset||<varname>||<value>}} -- Sets the specified local variable "
                + "to the specified value. Local variables are those that can be read by "
                + "using percent signs. For example, after {{lset||something||Hello world}} "
                + "is run in a factoid, %something% could be used and would be replaced with"
                + " \"Hello world\".";
    }
    
}
