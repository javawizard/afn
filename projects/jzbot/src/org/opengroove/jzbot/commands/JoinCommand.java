package org.opengroove.jzbot.commands;

import org.opengroove.jzbot.Command;
import org.opengroove.jzbot.JZBot;
import org.opengroove.jzbot.storage.Channel;

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
        if (!JZBot.isSuperop(hostname))
        {
            JZBot.bot.sendMessage(sender, "You are not a superop");
            return;
        }
        String name = arguments;
        Channel c = JZBot.storage.createChannel();
        c.setName(name);
        c.setTrigger("~");
        JZBot.storage.getChannels().add(c);
        JZBot.bot.joinChannel(name);
        JZBot.bot.sendMessage(sender, "Successful.");
        JZBot.bot.sendMessage(name, "Here I am (courtesy of " + sender + ")");
    }
}
