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
        JZBot.bot.sendMessage(pm ? sender : channel, "Reconnecting on request from "
            + sender);
        Thread.sleep(2000);
        JZBot.bot.disconnect();
    }
    
}
