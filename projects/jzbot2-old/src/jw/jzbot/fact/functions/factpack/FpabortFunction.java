package jw.jzbot.fact.functions.factpack;

import jw.jzbot.fact.ArgumentList;
import jw.jzbot.fact.FactContext;
import jw.jzbot.fact.FactpackInstallationException;
import jw.jzbot.fact.Function;
import jw.jzbot.fact.Sink;

public class FpabortFunction extends Function
{
    
    @Override
    public void evaluate(Sink sink, ArgumentList arguments, FactContext context)
    {
        throw new FactpackInstallationException(arguments.resolveString(0));
    }
    
    @Override
    public String getHelp(String topic)
    {
        return "Syntax: {fpabort|<message>} -- When used in a preinstall or preuninstall "
                + "factpack script, aborts installation or uninstallation, respectively, "
                + "of the factpack, and causes the specified message to be sent by the "
                + "bot. The message can contain newlines, in which case each line of "
                + "the message will be sent as its own message.";
    }
    
}
