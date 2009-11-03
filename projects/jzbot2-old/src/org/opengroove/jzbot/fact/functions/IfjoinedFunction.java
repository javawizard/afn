package org.opengroove.jzbot.fact.functions;

import org.jibble.pircbot.User;
import org.opengroove.jzbot.JZBot;
import org.opengroove.jzbot.fact.ArgumentList;
import org.opengroove.jzbot.fact.FactContext;
import org.opengroove.jzbot.fact.Function;

public class IfjoinedFunction extends Function
{
    
    @Override
    public String evaluate(ArgumentList arguments, FactContext context)
    {
        User[] users = JZBot.bot.getUsers(context.getChannel());
        boolean isJoined = false;
        for (User user : users)
        {
            if (user.getNick().equalsIgnoreCase(arguments.get(0)))
            {
                isJoined = true;
                break;
            }
        }
        if (isJoined)
            return arguments.get(1);
        else if (arguments.length() > 2)
            return arguments.get(2);
        else
            return "";
    }
    
    public String getName()
    {
        return "ifjoined";
    }
    
    @Override
    public String getHelp(String topic)
    {
        return "Syntax: {{ifjoined||<nick>||<trueaction>||<falseaction>}} -- Evaluates to "
                + "<trueaction> if the user <nick> is currently a member of the channel that "
                + "the factoid is being run on, and <falseaction> if the user is not currently "
                + "a member of that channel or if the user is offline. <falseaction> is optional."
                + " if <trueaction> and <falseaction> are bot missing, then this \n"
                + "function evaluates to 1 if the user is joined to the channel "
                + "and 0 if the user is not joined.";
    }
    
}
