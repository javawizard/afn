package org.opengroove.jzbot.fact.functions;

import org.opengroove.jzbot.JZBot;
import org.opengroove.jzbot.fact.ArgumentList;
import org.opengroove.jzbot.fact.FactContext;
import org.opengroove.jzbot.fact.Function;

public class TemppartFunction extends Function
{
    
    @Override
    public String evaluate(ArgumentList arguments, FactContext context)
    {
        // FIXME: increment sent message count
        JZBot.bot.partChannel(arguments.get(0));
        return "";
    }
    
    @Override
    public String getHelp(String topic)
    {
        return "Syntax: {{temppart||<channel>||<reason?}} -- Causes the bot to leave "
                + "the specified channel, but without removing the channel from the bot's "
                + "auto-join list.";
    }
    
}
