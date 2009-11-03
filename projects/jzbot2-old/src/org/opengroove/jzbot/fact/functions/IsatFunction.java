package org.opengroove.jzbot.fact.functions;

import java.util.Arrays;

import org.opengroove.jzbot.JZBot;
import org.opengroove.jzbot.fact.ArgumentList;
import org.opengroove.jzbot.fact.FactContext;
import org.opengroove.jzbot.fact.Function;

public class IsatFunction extends Function
{
    
    @Override
    public String evaluate(ArgumentList arguments, FactContext context)
    {
        return Arrays.asList(JZBot.bot.getChannels()).contains(arguments.get(0)) ? "1"
                : "0";
    }
    
    @Override
    public String getHelp(String topic)
    {
        return "Syntax: {{isat||<channel>}} -- Evaluates to 1 if the bot is currently "
                + "at the specified channel, or 0 if the bot is not currently joined "
                + "to the specified channel. This does not take into account whether "
                + "or not the bot is currently on the bot's auto-join list; it simply "
                + "takes into account what channels the IRC server would see the bot \n"
                + "as being on. To find out if a particular channel is on the bot's "
                + "auto-join list, use {{isautojoin}} instead.";
    }
    
}
