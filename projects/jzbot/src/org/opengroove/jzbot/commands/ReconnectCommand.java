package org.opengroove.jzbot.commands;

import org.opengroove.jzbot.Command;
import org.opengroove.jzbot.JZBot;

public class ReconnectCommand implements Command
{
    
    public String getName()
    {
        return "reconnect";
    }
    
    public void run(String channel, boolean pm, String sender, String hostname,
        String arguments)
    {
        JZBot.bot.verifySuperop(hostname);
    }
    
}
