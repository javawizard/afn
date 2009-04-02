package org.opengroove.jzbot.commands;

import org.opengroove.jzbot.Command;
import org.opengroove.jzbot.JZBot;
import org.opengroove.jzbot.ResponseException;
import org.opengroove.jzbot.storage.Channel;
import org.opengroove.jzbot.storage.Factoid;

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
        /*
         * command is something like create, delete, isglobal, etc., and
         * afterCommand is the rest
         */
        Channel c = null;
        if (!isGlobal)
            c = JZBot.storage.getChannel(channel);
        if (command.equals("create"))
        {
            verifyOpSuperop(isGlobal, channel, hostname);
            if (afterCommand.equals(""))
                throw new ResponseException("You need to specify the factoid");
            String[] argumentsTokenized2 = afterCommand.split(" ", 2);
            if (argumentsTokenized2.length != 2)
                throw new RuntimeException("You need to specify the factoid itself");
            String factoidName = argumentsTokenized2[0];
            String factoidContents = argumentsTokenized2[1];
            if (c != null && c.getFactoid(factoidName) != null)
                throw new ResponseException(
                    "That factoid already exists as a channel-specific factoid");
            else if (c == null && JZBot.storage.getFactoid(factoidName) != null)
                throw new ResponseException(
                    "That factoid already exists as a global factoid");
            /*
             * The factoid does not exist. Let's create it.
             */
            Factoid f = JZBot.storage.createFactoid();
            f.setCreator(hostname);
            f.setName()
        }
    }
    
    /**
     * Verifies that this is a superop if isGlobal is true. Otherwise, verifies
     * that this is an op.
     * 
     * @param isGlobal
     * @param channel
     * @param hostname
     */
    private void verifyOpSuperop(boolean isGlobal, String channel, String hostname)
    {
        if (isGlobal)
            JZBot.bot.verifySuperop(hostname);
        else
            JZBot.bot.verifyOp(channel, hostname);
    }
}
