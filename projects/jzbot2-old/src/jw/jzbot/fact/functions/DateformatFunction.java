package jw.jzbot.fact.functions;

import java.util.Date;

import jw.jzbot.fact.ArgumentList;
import jw.jzbot.fact.FactContext;
import jw.jzbot.fact.Function;
import jw.jzbot.fact.Sink;

public class DateformatFunction extends Function
{
    
    @Override
    public void evaluate(Sink sink, ArgumentList arguments, FactContext context)
    {
        sink.write(new Date(Long.parseLong(arguments.getString(0))).toString());
    }
    
    @Override
    public String getHelp(String topic)
    {
        return "Syntax: {dateformat|<value>} -- Formats <value>, which should be "
                + "a number of milliseconds since Midnight January 1, 1970 UTC, as "
                + "a human-readable date string. If you want a custom date format "
                + "instead of the default one that this function provides, consider "
                + "using {format} with a custom format string instead "
                + "of {dateformat}.";
    }
    
    public String getName()
    {
        return "dateformat";
    }
    
}
