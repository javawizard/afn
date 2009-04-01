package org.opengroove.jzbot.commands;

import org.opengroove.jzbot.Command;
import org.opengroove.jzbot.JZBot;

public class JoinCommand implements Command
{
    
    public String getName()
    {
        return "join";
    }
    
    public void run(String channel, boolean pm, String sender, String hostname,
        String arguments)
    {
        if (!pm)
        {
            JZBot.bot.sendMessage(sender, "Join only works in a pm");
            return;
        }
        String name = arguments;
        
    }
    
}
