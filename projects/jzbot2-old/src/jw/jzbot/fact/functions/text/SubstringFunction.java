package jw.jzbot.fact.functions.text;

import jw.jzbot.fact.ArgumentList;
import jw.jzbot.fact.FactContext;
import jw.jzbot.fact.Function;
import jw.jzbot.fact.Sink;

public class SubstringFunction extends Function
{
    
    @Override
    public void evaluate(Sink sink, ArgumentList arguments, FactContext context)
    {
        String s = arguments.resolveString(2);
        int start = Integer.parseInt(arguments.resolveString(0));
        int end = Integer.parseInt(arguments.resolveString(1));
        if (start < 0)
            start = 0;
        if (end > s.length())
            end = s.length();
        sink.write(s.substring(start, end));
    }
    
    @Override
    public String getHelp(String topic)
    {
        return "Syntax: {substring|<start>|<end>|<text>} -- Evaluates to a substring of "
                + "<text>, which starts at the index specified by <start> and ends at <end>. "
                + "If the specified indexes are out of bounds, they will be changed to be "
                + "within bounds. Indexes are 0-based, with start being inclusive and "
                + "end being exclusive. For example, {substring|3|6|0123456789} evaluates "
                + "to \"345\".";
    }
    
}
