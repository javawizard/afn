package org.opengroove.jzbot.commands;

import org.opengroove.jzbot.Command;
import org.opengroove.jzbot.JZBot;
import org.opengroove.jzbot.storage.Channel;

public class TriggerCommand implements Command
{
    
    public String getName()
    {
        return "trigger";
    }
    
    public void run(String channel, boolean pm, String sender, String hostname,
        String arguments)
    {
        if (channel == null)
        {
            JZBot.bot.sendMessage(pm ? sender : channel, "Need a channel");
            return;
        }
        Channel c = JZBot.storage.getChannel(channel);
        
        if (arguments.equals(""))
        {
            
        }
        if (!JZBot.isOp(channel, hostname))
        {
            JZBot.bot.sendMessage(pm ? sender : channel, "You are not an op here");
            return;
        }
        
    }
    
}
