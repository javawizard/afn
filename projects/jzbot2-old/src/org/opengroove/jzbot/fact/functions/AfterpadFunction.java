package org.opengroove.jzbot.fact.functions;

import org.opengroove.jzbot.fact.ArgumentList;
import org.opengroove.jzbot.fact.FactContext;
import org.opengroove.jzbot.fact.Function;

public class AfterpadFunction extends Function
{
    
    @Override
    public String evaluate(ArgumentList arguments, FactContext context)
    {
        int number = Integer.parseInt(arguments.get(0));
        String c = arguments.get(1);
        String value = arguments.get(2);
        return pad(number,c,value);
    }
    
    public static String pad(int targetLength, String toPadWith, String value)
    {
        while (value.length() < targetLength)
        {
            value = value + toPadWith;
        }
        return value;
    }

    @Override
    public String getHelp(String topic)
    {
        return "Syntax: {{afterpad||<number>||<char>||<value>}} -- Evaluates to <value>, but "
                + "with <char> (which must be a single character) appended until the "
                + "resulting length is at least equal to <number>. For example, "
                + "{{pad||7||0||1234}} would evaluate to \"1234000\".";
    }
    
    public String getName()
    {
        return "pad";
    }
    
}
