package org.opengroove.jzbot.commands;

import org.opengroove.jzbot.Command;
import org.opengroove.jzbot.JZBot;
import org.opengroove.jzbot.storage.Channel;

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
        JZBot.bot.sendMessage(pm ? sender : channel, "Shutdown has been scheduled.");
        long sleepDuration = 5000;
        for (Channel c : JZBot.storage.getChannels().isolate())
        {
            
        }
    }
    
}
