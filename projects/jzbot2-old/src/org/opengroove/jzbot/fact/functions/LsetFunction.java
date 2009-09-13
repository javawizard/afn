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
    
}
