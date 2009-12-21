package jw.jzbot.commands;

import jw.jzbot.Command;
import jw.jzbot.JZBot;
import jw.jzbot.Messenger;
import jw.jzbot.ServerUser;
import jw.jzbot.storage.Channel;
import jw.jzbot.storage.Server;

public class TriggerCommand implements Command
{
    
    public String getName()
    {
        return "trigger";
    }
    
    public void run(String server, String channel, boolean pm, ServerUser sender,
            Messenger source, String arguments)
    {
        if (channel == null)
        {
            source
                    .sendMessage("You need to run the trigger command in the context of a channel.");
            return;
        }
        Server s = JZBot.storage.getServer(server);
        Channel c = s.getChannel(channel);
        if (arguments.equals(""))
        {
            source.sendMessage("The current trigger is " + c.getTrigger());
            return;
        }
        if (!sender.isSuperop())
        {
            source.sendMessage("You are not an op here, so you can't change the trigger");
            return;
        }
        c.setTrigger(arguments);
        source.sendMessage("Trigger set to " + arguments);
    }
    
}
