package org.opengroove.jzbot.fact.functions;

import java.util.HashMap;
import java.util.Map;

import org.opengroove.jzbot.JZBot;
import org.opengroove.jzbot.fact.ArgumentList;
import org.opengroove.jzbot.fact.Deferred;
import org.opengroove.jzbot.fact.FactContext;
import org.opengroove.jzbot.fact.Function;
import org.opengroove.jzbot.storage.MapEntry;

public class TransferFunction extends Function
{
    
    @Override
    public String evaluate(ArgumentList arguments, FactContext context)
    {
        Map<String, String> newMap = new HashMap<String, String>();
        String from = arguments.get(0);
        String to = arguments.get(1);
        String regex = arguments.get(2);
        String varname = null;
        Deferred namegen = null;
        if (arguments.length() > 4)
        {
            varname = arguments.get(3);
            namegen = arguments.getDeferred(4);
        }
        from = from.substring(0, 1).toLowerCase();
        to = to.substring(0, 1).toLowerCase();
        /*
         * First step is getting the variables and the values that we're going to move to
         * the new variable type.
         */
        if (from.equals("p"))
        {
            for (MapEntry entry : JZBot.storage.getPersistentVariables().isolate())
            {
                String key = entry.getKey();
                if (key.matches(regex))
                    newMap.put(key, entry.getValue());
            }
        }
        else
        {
            assert from.equals("g") || from.equals("l") || from.equals("c") : "From "
                    + "(substring 0,1) was " + from;
            Map<String, String> source = from.equals("g") ? context.getGlobalVars() : from
                    .equals("l") ? context.getLocalVars() : context.getChainVars();
            for (Map.Entry<String, String> entry : source.entrySet())
            {
                if (entry.getKey().matches(regex))
                    newMap.put(entry.getKey(), entry.getValue());
            }
        }
        /*
         * Second step is running the namegen stuff.
         */
        Map<String, String> namedMap;
        if (varname != null)
        {
            namedMap = new HashMap<String, String>();
            String previousValue = context.getLocalVars().get(varname);
            for (Map.Entry<String, String> entry : newMap.entrySet())
            {
                context.getLocalVars().put(varname, entry.getKey());
                String newName = namegen.resolve();
                if (!newName.equals(""))
                    namedMap.put(newName, entry.getValue());
            }
            if (previousValue == null)
                context.getLocalVars().remove(varname);
            else
                context.getLocalVars().put(varname, previousValue);
        }
        else
        {
            namedMap = newMap;
        }
        return "";
    }
    
    @Override
    public String getHelp(String topic)
    {
        return "Syntax: {{transfer||<from>||<to>||<regex>||<varname>||<namegen>}} -- "
                + "Transfers a set of variables from one variable type to another variable "
                + "type, or bulk-renames a set of variables. <from> and <to> are one of "
                + "\"global\", \"local\", \"persistent\", or \"chain\", or the first "
                + "letter of one of those. They specify the type of variable to copy \n"
                + "from and the type of variable to copy to. All variables whose names "
                + "match <regex> will be copied. <varname> and <namegen> are optional, "
                + "and if present, for each variable copied, a local variable named "
                + "<varname> will be set to the name of the variable to copy, and "
                + "<namegen> will be run. <namegen> should then evaluate to the new \n"
                + "name for the variable. If <varname> and <namegen> are absent, the "
                + "variable's original name is kept. If <namegen> evaluates to the "
                + "empty string, the variable is not copied.";
    }
    
}
