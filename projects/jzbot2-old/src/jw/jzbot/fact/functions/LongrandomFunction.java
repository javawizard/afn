package jw.jzbot.fact.functions;

import jw.jzbot.fact.ArgumentList;
import jw.jzbot.fact.FactContext;
import jw.jzbot.fact.Function;
import jw.jzbot.fact.Sink;

public class LongrandomFunction extends Function
{
    
    @Override
    public void evaluate(Sink sink, ArgumentList arguments, FactContext context)
    {
        sink.write(("" + Math.random() + "" + Math.random()).replaceAll("[^0-9]", ""));
    }
    
    @Override
    public String getHelp(String topic)
    {
        return "Syntax: {longrandom} -- Generates a random string of digits. The length of this "
                + "string is generally around 40 characters, and is guaranteed not to be longer "
                + "than 42 characters. This is intended for instances where some sort of unique "
                + "ID is needed.";
    }
    
}
