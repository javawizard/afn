package jw.jzbot.fact.functions;

import jw.jzbot.JZBot;
import jw.jzbot.fact.ArgumentList;
import jw.jzbot.fact.FactContext;
import jw.jzbot.fact.Function;
import jw.jzbot.fact.Sink;
import jw.jzbot.help.HelpProvider;
import jw.jzbot.help.HelpSystem;

public class HelpFunction extends Function
{
    
    @Override
    public void evaluate(Sink sink, ArgumentList arguments, FactContext context)
    {
        String pagename = arguments.resolveString(0);
        for (HelpProvider provider : HelpSystem.getProviders())
        {
            String possible = provider.getPage(pagename);
            if (possible != null)
            {
                sink.write(possible);
                break;
            }
        }
    }
    
    @Override
    public String getHelp(String topic)
    {
        return "Syntax: {help|<page>} -- Evaluates to the contents of the specified help "
            + "page. <page> should be a page formatted so that sending \"help <page>\" "
            + "to the bot in a pm would get the relevant help page. The resulting help "
            + "page can contain newlines. If the specified help page does not exist, "
            + "{help} evaluates to nothing.\n"
            + "{help|} evaluates to the contents of the top level help page (IE "
            + "the one that you see if you pm \"help\" to the bot). Also, "
            + "percentHELPCMDpercent (with \"percent\" replaced with \"%\") may "
            + "appear in the result, which \n"
            + "should be translated to \"~help\" or \"/msg <bot>"
            + " help\", depending on where the message was sent from.";
    }
    
}
