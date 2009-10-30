package org.opengroove.jzbot.commands;

import java.util.HashMap;
import java.util.Map;

import org.opengroove.jzbot.Command;
import org.opengroove.jzbot.JZBot;
import org.opengroove.jzbot.fact.FactContext;
import org.opengroove.jzbot.fact.FactEntity;
import org.opengroove.jzbot.fact.FactParser;
import org.opengroove.jzbot.fact.FactQuota;

public class ExecCommand implements Command
{
    
    @Override
    public String getName()
    {
        return "exec";
    }
    
    @Override
    public void run(String channel, boolean pm, String sender, String hostname,
            String arguments)
    {
        JZBot.verifyOp(channel, hostname);
        FactEntity entity = FactParser.parse(arguments, "__internal_exec");
        FactContext context = new FactContext();
        context.setChannel(channel);
        context.setSender(sender);
        context.setSelf(JZBot.bot.getNick());
        context.setQuota(new FactQuota());
        Map<String, String> vars = new HashMap<String, String>();
        vars.put("channel", channel);
        vars.put("0", sender);
        vars.put("who", sender);
        vars.put("source", pm ? sender : channel);
        context.setLocalVars(vars);
        context.setGlobalVars(JZBot.globalVariables);
        String result = entity.resolve(context);
        if (result.equals(""))
            result = "(no result)";
        JZBot.bot.sendMessage(pm ? sender : channel, result);
    }
    
}
