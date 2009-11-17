package jw.jzbot.fact.functions.text;

import jw.jzbot.fact.ArgumentList;
import jw.jzbot.fact.FactContext;
import jw.jzbot.fact.Function;

public class ContainsFunction extends Function
{
    
    @Override
    public String evaluate(ArgumentList arguments, FactContext context)
    {
        return arguments.get(1).contains(arguments.get(0)) ? "1" : "0";
    }
    
    @Override
    public String getHelp(String topic)
    {
        return "Syntax: {{contains||<substring>||<string>}} -- Evaluates to 1 if <string> "
                + "contains <substring> anywhere in it, or 0 if it doesn't.";
    }
    
}
