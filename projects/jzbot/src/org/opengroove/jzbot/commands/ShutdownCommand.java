package org.opengroove.jzbot.commands;

import org.opengroove.jzbot.Command;
import org.opengroove.jzbot.JZBot;

public class ShutdownCommand implements Command
{
    
    public String getName()
    {
        return "shutdown";
    }
    
    public void run(String channel, boolean pm, String sender, String hostname,
        String arguments)
    {
        if (!JZBot.isSuperop(hostname))
        {
            JZBot.bot.sendMessage(pm ? sender : channel, "You're not a superop.");
            return;
        }
    }
    
}
