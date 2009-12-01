package jw.jzbot.commands;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;

import jw.jzbot.Command;
import jw.jzbot.ConfigVars;
import jw.jzbot.HelpProvider;
import jw.jzbot.JZBot;
import jw.jzbot.ResponseException;
import jw.jzbot.storage.Channel;
import jw.jzbot.utils.JZUtils;
import jw.jzbot.utils.Pastebin;
import jw.jzbot.utils.Pastebin.Duration;

public class HelpCommand implements Command
{
    
    public String getName()
    {
        return "help";
    }
    
    public void run(String channel, boolean pm, String sender, String hostname,
            String arguments)
    {
        if (ConfigVars.helpinpm.get().equals("1") && !pm)
        {
            if (arguments.equals("functions"))
            {
            	throw new ResponseException(
            			"You're not allowed to run the help command at a channel. " +
            			"You can use " +
            			"http://code.google.com/p/jzbot/wiki/FactoidFunctions " +
            			"or try sending \"help\" in a pm to the bot instead.");
            }
            else
            {
            throw new ResponseException(
                    "You're not allowed to run the help command at a channel. "
                            + "Try sending \"help\" in a pm to the bot instead.");
            }
        }
        ArrayList<String> subpages = new ArrayList<String>();
        String page = arguments;
        String text = null;
        boolean allSubpages = false;
        if (page.endsWith(" --") || page.equals("--"))
        {
            allSubpages = true;
            if (page.equals("--"))
                page = "";
            else
                page = page.substring(0, page.length() - " --".length());
        }
        for (HelpProvider provider : JZBot.helpProviders)
        {
            String possibleText = provider.getPage(page);
            if (possibleText != null)
                text = possibleText;
            String[] possibleSubpages = provider.listPages(page);
            if (possibleSubpages != null)
                subpages.addAll(Arrays.asList(possibleSubpages));
        }
        Collections.sort(subpages);
        if (text == null)
            throw new ResponseException("No such help page");
        String helpCommand;
        if (pm)
            helpCommand = "/msg " + JZBot.bot.getNick() + " help";
        else
        {
            Channel c = JZBot.storage.getChannel(channel);
            if (c != null)
                helpCommand = c.getTrigger() + "help";
            else
                helpCommand = "~trigger";
        }
        text = text.replace("%HELPCMD%", helpCommand);
        if (!allSubpages)
        {
            String[] messages = text.split("\n");
            for (String s : messages)
            {
                if (!s.trim().equals(""))
                    JZBot.bot.sendMessage(pm ? sender : channel, s);
            }
            String pageWithSpace = page;
            if (!pageWithSpace.trim().equals(""))
                pageWithSpace = " " + pageWithSpace;
            String startText = (subpages.size() > 0 ? "Subpages (\"" + helpCommand
                    + pageWithSpace + " <pagename>\" to show a page): " : "No subpages.");
            String prefix = "---> ";
            String[] delimited = JZUtils.delimitedLengthRestricted(subpages
                    .toArray(new String[0]), "   ", 320);
            boolean sentFirst = false;
            if (delimited.length > 0)
            {
                for (String s : delimited)
                {
                    if (sentFirst)
                    {
                        JZBot.bot.sendMessage(pm ? sender : channel, prefix + s);
                    }
                    else
                    {
                        sentFirst = true;
                        if (subpages.size() > 0)// disables "no subpages" message for now
                            JZBot.bot.sendMessage(pm ? sender : channel, prefix + startText
                                    + s);
                    }
                }
            }
            else
            {
                JZBot.bot.sendMessage(pm ? sender : channel, prefix + startText);
            }
        }
        else
        // if(allSubpages)
        {
            StringBuffer buffer = new StringBuffer();
            buffer.append("All immediate subpages of \"" + page + "\":\n\n");
            for (String subpage : subpages)
            {
                String subtext = "";
                for (HelpProvider provider : JZBot.helpProviders)
                {
                    String possibleText = provider.getPage(page + " " + subpage);
                    if (possibleText != null)
                        subtext = possibleText;
                }
                buffer.append(subpage).append(":\n");
                buffer.append(subtext).append("\n\n");
            }
            JZBot.bot.sendMessage(pm ? sender : channel, "All subpages of \""
                    + page
                    + "\": "
                    + Pastebin.createPost("jzbot", buffer.toString(), Duration.DAY, null,
                            null));
        }
    }
}
