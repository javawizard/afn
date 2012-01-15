package jw.jzbot.fact.functions;

import java.util.Formatter;

import jw.jzbot.fact.ArgumentList;
import jw.jzbot.fact.FactContext;
import jw.jzbot.fact.Function;
import jw.jzbot.fact.Sink;
import jw.jzbot.fact.SinkAppendable;

public class FormatFunction extends Function
{
    
    @Override
    public void evaluate(Sink sink, ArgumentList arguments, FactContext context)
    {
        Object[] objects = new Object[arguments.length() - 1];
        String format = arguments.resolveString(0);
        for (int i = 0; i < objects.length; i++)
        {
            String s = arguments.resolveString(i + 1);
            Object o = null;
            try
            {
                if (o == null)
                    o = new Long(s);
            }
            catch (NumberFormatException e)
            {
            }
            try
            {
                if (o == null)
                    o = new Double(s);
            }
            catch (NumberFormatException e)
            {
            }
            if (o == null)
                o = s;
            objects[i] = o;
        }
        SinkAppendable output = new SinkAppendable(sink);
        Formatter formatter = new Formatter(output);
        formatter.format(format, objects);
    }
    
    @Override
    public String getHelp(String topic)
    {
        return "Syntax: {format|<formatstring>|<arg1>|<arg2>|...} -- Applies C-style "
                + "printf formatting to <formatstring> with <arg1>, <arg2>, etc. being "
                + "the format arguments. This could be considered an equivalent to "
                + "C's printf(<formatstring>,<arg1>,<arg2>,...), but output, instead "
                + "of being sent to stdout, is put into the factoid.\n"
                + "Arguments that can be parsed as whole numbers are passed to "
                + "the formatting method as 64-bit signed integers. Arguments that can be"
                + " parsed as fractional "
                + "numbers (including numbers with a decimal point but that only have zeros "
                + "after it) are passed as 64-bit floating point numbers. Other arguments "
                + "are passed as strings.";
    }
    
    public String getName()
    {
        return "format";
    }
    
}
