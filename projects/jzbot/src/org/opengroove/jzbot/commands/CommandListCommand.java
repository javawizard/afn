package org.opengroove.jzbot.commands;

import org.opengroove.jzbot.Command;
import org.opengroove.jzbot.JZBot;

public class CommandListCommand implements Command
{
    
    public String getName()
    {
        return "commandlist";
    }
    
    public void run(String channel, boolean pm, String sender, String hostname,
        String arguments)
    {
        JZBot.bot.sendMessage(pm ? sender : channel, "Start of command list");
        for (String name : JZBot.commands.keySet())
        {
            JZBot.bot.sendMessage(pm ? sender : channel, name);
        }
        JZBot.bot.sendMessage(pm ? sender : channel, "End of command list");
    }
    
}
