package jw.jzbot.fact.functions;

import jw.jzbot.fact.ArgumentList;
import jw.jzbot.fact.FactContext;
import jw.jzbot.fact.Function;

public class FirstFunction extends Function
{
    
    @Override
    public String evaluate(ArgumentList arguments, FactContext context)
    {
        for (int i = 0; i < arguments.length(); i++)
        {
            if (!"".equals(arguments.get(i)))
                return arguments.get(i);
        }
        return "";
    }
    
    @Override
    public String getHelp(String topic)
    {
        return "Syntax: {{first||<arg1>||<arg2>||...}} -- Evaluates to the first argument "
                + "that is not the empty string.";
    }
    
}
