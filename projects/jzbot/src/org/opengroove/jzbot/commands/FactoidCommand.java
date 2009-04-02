package org.opengroove.jzbot.commands;

import org.opengroove.jzbot.Command;
import org.opengroove.jzbot.JZBot;

public class FactoidCommand implements Command
{
    
    public String getName()
    {
        return "factoid";
    }
    
    public void run(String channel, boolean pm, String sender, String hostname,
        String arguments)
    {
        boolean isGlobal = false;
        if (arguments.startsWith("global "))
        {
            isGlobal = true;
            arguments = arguments.substring("global ".length());
        }
        String[] argumentsTokenized1 = arguments.split(" ", 2);
        String command = argumentsTokenized1[0];
        if ((!isGlobal) && (channel == null) && (!command.equalsIgnoreCase("isglobal")))
        {
            JZBot.bot.sendMessage(pm ? sender : channel,
                "For non-global commands, you must specify "
                    + "a channel (unless it is the isglobal command)");
            return;
        }
        String afterCommand =
            (argumentsTokenized1.length > 1) ? argumentsTokenized1[1] : "";
    }
}
