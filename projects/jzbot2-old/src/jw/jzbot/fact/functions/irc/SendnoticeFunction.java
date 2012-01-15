package jw.jzbot.fact.functions.irc;

import jw.jzbot.ConnectionWrapper;
import jw.jzbot.JZBot;
import jw.jzbot.fact.ArgumentList;
import jw.jzbot.fact.FactContext;
import jw.jzbot.fact.Function;
import jw.jzbot.fact.Sink;
import jw.jzbot.fact.exceptions.FactoidException;

public class SendnoticeFunction extends Function
{
    
    @Override
    public void evaluate(Sink sink, ArgumentList arguments, FactContext context)
    {
        String to = arguments.resolveString(0);
        String serverName = JZBot.extractRelativeServer(to, context);
        ConnectionWrapper con = JZBot.getCheckedConnection(serverName);
        if (!context.checkedGetConnection().getConnection().isConnected())
            throw new FactoidException("Can't send messages when the bot is disconnected");
        String message = arguments.resolveString(1);
        context.incrementMessageCount();
        context.checkedGetConnection().getConnection().sendNotice(to, message);
    }
    
    public String getName()
    {
        return "sendnotice";
    }
    
    @Override
    public String getHelp(String topic)
    {
        return "Syntax: {sendnotice|<to>|<message>} -- Sends the specified notice message to "
                + "the specified recipient, which can be a channel or a nickname.";
    }
    
}
