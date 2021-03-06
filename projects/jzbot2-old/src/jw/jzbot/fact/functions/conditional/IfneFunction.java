package jw.jzbot.fact.functions.conditional;

import jw.jzbot.fact.ArgumentList;
import jw.jzbot.fact.FactContext;
import jw.jzbot.fact.Function;
import jw.jzbot.fact.Sink;

public class IfneFunction extends Function
{
    @Override
    public void evaluate(Sink sink, ArgumentList arguments, FactContext context)
    {
        if (!arguments.resolveString(0).trim().equals(""))
            arguments.resolve(1, sink);
        else if (arguments.length() > 2)
            arguments.resolve(2, sink);
    }
    
    @Override
    public String getHelp(String topic)
    {
        return "Syntax: {ifne|<compare>|<trueaction>|<falseaction>} -- Evaluates "
                + "to <trueaction> if <compare> is not empty and is not made up only of "
                + "whitespace, or <falseaction> if <compare> is empty or made "
                + "up entirely of whitespace. <falseaction> is optional, and if it's not "
                + "present {ifne} will act as if <falseaction> were empty. This function "
                + "is the opposite of {ife}.";
    }
    
}
