package org.opengroove.jzbot.commands;

import org.opengroove.jzbot.Command;
import org.opengroove.jzbot.JZBot;
import org.opengroove.jzbot.ResponseException;
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
        if (JZBot.storage.getChannel(name) != null)
        {
            Channel c = JZBot.storage.getChannel(name);
            if (c.isSuspended())
            {
                c.setSuspended(false);
                JZBot.bot.sendMessage(pm ? sender : channel,
                    "Ok, I'll come back to that channel.");
                JZBot.bot.joinChannel(name);
                JZBot.bot.sendMessage(name, "I've come back (courtesy of " + sender
                    + ")");
                JZBot.bot.sendMessage(name, "I still remember all of my factoids and "
                    + "settings from this channel, and I'm still only "
                    + "allowing ops to create factoids here.");
            }
            else
            {
                throw new ResponseException("I'm already a member of that channel.");
            }
            return;
        }
        Channel c = JZBot.storage.createChannel();
        c.setName(name);
        c.setTrigger("~");
        JZBot.storage.getChannels().add(c);
        JZBot.bot.joinChannel(name);
        JZBot.bot.sendMessage(sender, "Successful.");
        JZBot.bot.sendMessage(name, "Here I am (courtesy of " + sender
            + "). I'm only allowing ops to create factoids here.");
    }
}
