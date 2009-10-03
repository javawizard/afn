package org.opengroove.jzbot.fact.functions;

import java.util.ArrayList;
import java.util.Arrays;

import net.sf.opengroove.common.utils.StringUtils;

import org.opengroove.jzbot.HelpProvider;
import org.opengroove.jzbot.JZBot;
import org.opengroove.jzbot.fact.ArgumentList;
import org.opengroove.jzbot.fact.FactContext;
import org.opengroove.jzbot.fact.Function;

public class HelplistFunction extends Function
{
    
    @Override
    public String evaluate(ArgumentList arguments, FactContext context)
    {
        ArrayList<String> list = new ArrayList<String>();
        String pagename = arguments.get(0);
        for (HelpProvider provider : JZBot.helpProviders)
        {
            String[] possible = provider.listPages(pagename);
            if (possible != null)
                list.addAll(Arrays.asList(possible));
        }
        return StringUtils.delimited(list.toArray(new String[0]), " ");
    }
    
    @Override
    public String getHelp(String topic)
    {
        return "Syntax: {{helplist||<page>}} -- Evaluates to a space-separated list "
                + "of all subpages of the help page <page>. This also means that you "
                + "can get a space-separated list of all functions allowed in "
                + "factoids by using {{helplist||functions}}. {{helplist||}} evaluates "
                + "to a list of top-level help pages (IE those that you would see \n"
                + "if you sent \"help\" in a pm to the bot).";
    }
    
}
