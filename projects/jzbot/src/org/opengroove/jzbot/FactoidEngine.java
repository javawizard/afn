package org.opengroove.jzbot;

import java.util.HashMap;
import java.util.Map;

import org.opengroove.jzbot.storage.Factoid;

public class FactoidEngine
{
    /**
     * Runs the factoid specified, outputting to the string specified.
     * 
     * @param factoid
     *            The factoid to run
     * @param channel
     *            The channel that it was run on
     * @param sender
     *            The sender of the factoid request
     */
    public static String runFactoid(Factoid factoid, String channel, String sender,
        String[] args, Map<String, String> vars)
    {
        for (int i = 0; i < args.length; i++)
        {
            vars.put("" + (i + 1), args[i]);
        }
        String cAppend = "";
        for (int i = args.length - 1; i >= 0; i--)
        {
            cAppend = args[i] + ((i == args.length - 1) ? "" : " ") + cAppend;
            vars.put("" + (i + 1) + "-", cAppend);
        }
        vars.put("0", sender);
        String text = factoid.getValue();
        boolean isAction = false;
        StringBuffer result = new StringBuffer();
        while (text.length() > 0)
        {
            if (text.startsWith(" "))
            {
                text = text.substring(1);
                result.append(" ");
            }
            else if (text.startsWith("{{"))
            {
                int closeIndex = text.indexOf("}}");
                if (closeIndex == -1)
                    throw new RuntimeException("dangling command brace series");
                String toClose = text.substring(0, closeIndex + 2);
                text = text.substring(toClose.length());
                String commandString = toClose.substring(2, toClose.length() - 2);
                String[] commandStringTokens = commandString.split("\\|\\|", 2);
                String command = commandStringTokens[0];
                String[] arguments =
                    (commandStringTokens.length == 1) ? new String[0]
                        : commandStringTokens[1].split("\\|\\|");
                for (int i = 0; i < arguments.length; i++)
                {
                    arguments[i] = replaceVars(arguments[i], vars);
                }
                if (command.equals("action"))
                {
                    isAction = true;
                }
                else if (command.equals("firstvar"))
                {
                    for (int i = 1; i < arguments.length; i++)
                    {
                        if (!arguments[i].trim().equals(""))
                        {
                            vars.put(arguments[0], arguments[i]);
                            break;
                        }
                    }
                }
                else if (command.equals("random"))
                {
                    result.append(arguments[(int) (Math.random() * arguments.length)]);
                }
                else if (command.equals("ifeq"))
                {
                    if (arguments[0].equalsIgnoreCase(arguments[1]))
                        result.append(arguments[2]);
                    else if (arguments.length > 3)
                        result.append(arguments[3]);
                }
                else if (command.equals("ifneq"))
                {
                    if (!arguments[0].equalsIgnoreCase(arguments[1]))
                        result.append(arguments[2]);
                    else if (arguments.length > 3)
                        result.append(arguments[3]);
                }
                else if (command.equals("pm"))
                {
                    if (arguments.length < 2)
                        result
                            .append("Invalid argument number to pm, needs at least 2");
                    else
                    {
                        String message = arguments[arguments.length - 1];
                        for (int i = 0; i < (arguments.length - 1); i++)
                        {
                            JZBot.bot.sendMessage(arguments[i], message);
                        }
                    }
                }
                else if (command.equals("import"))
                {
                    Factoid f = null;
                    boolean channelSpecific = false;
                    if (channel != null)
                    {
                        f = JZBot.storage.getChannel(channel).getFactoid(arguments[0]);
                        if (f != null)
                            channelSpecific = true;
                    }
                    if (f == null)
                    {
                        f = JZBot.storage.getFactoid(arguments[0]);
                    }
                    if (f == null)
                        throw new RuntimeException("Invalid import factoid "
                            + arguments[0]);
                    String[] subargs = new String[arguments.length - 1];
                    System.arraycopy(arguments, 1, subargs, 0, subargs.length);
                    result.append(runFactoid(f, channelSpecific ? channel : null,
                        sender, subargs, new HashMap<String, String>()));
                }
                else if (command.equals(""))
                {
                    
                }
                else
                {
                    throw new RuntimeException("Invalid command " + command
                        + " in def " + commandString);
                }
            }
            else
            {
                String[] tokens = text.split(" ", 2);
                result.append(replaceVars(tokens[0], vars));
                if (tokens.length > 1)
                    result.append(" ");
                text = (tokens.length == 1) ? "" : tokens[1];
            }
        }
        // FIXME: actually deal with isAction, probably prepend result with
        // "<ACTION>"
        return result.toString();
    }
    
    private static String replaceVars(String text, Map<String, String> vars)
    {
        for (String key : vars.keySet())
        {
            String value = vars.get(key);
            text = text.replace("%" + key + "%", value);
        }
        text = text.replaceAll("\\%[^\\%]+%", "");
        return text;
    }
    
}
