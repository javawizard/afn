package org.opengroove.jzbot.commands;

import org.opengroove.jzbot.Command;
import org.opengroove.jzbot.JZBot;

public class OpCommand implements Command
{
    
    public String getName()
    {
        return "op";
    }
    
    public void run(String channel, boolean pm, String sender, String hostname,
        String arguments)
    {
        if (channel == null)
        {
            JZBot.bot.sendMessage(pm ? sender : channel,
                "You have to specify a channel.");
            return;
        }
        if (!JZBot.isOp(channel, sender))
        {
            JZBot.bot.sendMessage(pm ? sender : channel, "You're not an op here.");
            return;
        }
        String[] tokens = arguments.split(" ", 2);
        String subcommand = tokens[0];
        if (subcommand.equals("list"))
        {
            
        }
        else if (subcommand.equals("add"))
        {
            
        }
        else if (subcommand.equals("delete"))
        {
            
        }
        else
        {
        }
    }
}
