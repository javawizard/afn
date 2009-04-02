package org.opengroove.jzbot;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;

import net.sf.opengroove.common.proxystorage.ProxyStorage;

import org.jibble.pircbot.IrcException;
import org.jibble.pircbot.NickAlreadyInUseException;
import org.jibble.pircbot.PircBot;
import org.opengroove.jzbot.commands.CommandListCommand;
import org.opengroove.jzbot.commands.ConfigCommand;
import org.opengroove.jzbot.commands.FactoidCommand;
import org.opengroove.jzbot.commands.HelpCommand;
import org.opengroove.jzbot.commands.JoinCommand;
import org.opengroove.jzbot.commands.JoinMessageCommand;
import org.opengroove.jzbot.commands.LeaveCommand;
import org.opengroove.jzbot.commands.MMCommand;
import org.opengroove.jzbot.commands.OpCommand;
import org.opengroove.jzbot.commands.ReconnectCommand;
import org.opengroove.jzbot.commands.RouletteCommand;
import org.opengroove.jzbot.commands.SayCommand;
import org.opengroove.jzbot.commands.ShutdownCommand;
import org.opengroove.jzbot.commands.SuperopCommand;
import org.opengroove.jzbot.commands.TTTCommand;
import org.opengroove.jzbot.commands.TriggerCommand;
import org.opengroove.jzbot.commands.WeatherCommand;
import org.opengroove.jzbot.storage.*;

/**
 * jzbot authenticates off of hostmask.
 */
public class JZBot extends PircBot
{
    public static final HashMap<String, Command> commands =
        new HashMap<String, Command>();
    public static final JZBot bot = new JZBot();
    // numeric 320: is signed on as account
    private static ProxyStorage<Storage> proxyStorage;
    public static Storage storage;
    
    public static Config config;
    public static boolean isRunning;
    
    public static void main(String[] args) throws Throwable
    {
        bot.start();
    }
    
    private static void loadCommands()
    {
        loadCommand(new CommandListCommand());
        loadCommand(new ConfigCommand());
        loadCommand(new FactoidCommand());
        loadCommand(new HelpCommand());
        loadCommand(new JoinCommand());
        loadCommand(new JoinMessageCommand());
        loadCommand(new LeaveCommand());
        loadCommand(new MMCommand());
        loadCommand(new OpCommand());
        loadCommand(new ReconnectCommand());
        loadCommand(new RouletteCommand());
        loadCommand(new SayCommand());
        loadCommand(new ShutdownCommand());
        loadCommand(new SuperopCommand());
        loadCommand(new TriggerCommand());
        loadCommand(new TTTCommand());
        loadCommand(new WeatherCommand());
    }
    
    private static void loadCommand(Command command)
    {
        commands.put(command.getName(), command);
    }
    
    private void start() throws Throwable
    {
        loadCommands();
        proxyStorage = new ProxyStorage<Storage>(Storage.class, new File("storage/db"));
        storage = proxyStorage.getRoot();
        config = storage.getConfig();
        if (config == null)
        {
            config = storage.createConfig();
            storage.setConfig(config);
        }
        if (config.getNick() == null || config.getPassword() == null
            || config.getServer() == null)
        {
            System.out.println("No connect info specified. JZBot will exit.");
            System.exit(0);
        }
        bot.setLogin(config.getNick());
        bot.setName(config.getNick());
        System.out.println("connecting");
        bot.connect(config.getServer(), config.getPort(), config.getPassword());
        System.out.println("connected");
    }
    
    protected void onJoin(String channel, String sender, String login, String hostname)
    {
        Channel chan = storage.getChannel(channel);
        if (chan == null)
            return;
        if (sender.equals(getName()))
        {
            Factoid f = chan.getFactoid("selfjoin");
            if (f != null)
            {
                sendMessage(channel, runFactoid(f, channel, sender, new String[0],
                    new HashMap<String, String>()));
            }
            return;
        }
        if (chan.getJoinFactoid() != null)
        {
            System.out.println("join factoid");
            String jfString = chan.getJoinFactoid();
            System.out.println("string " + jfString);
            if (jfString == null)
                jfString = "";
            String[] factoidList = jfString.split(" ");
            for (String fName : factoidList)
            {
                if (!fName.equals(""))
                {
                    System.out.println("issuing on join " + fName);
                    Factoid factoid = chan.getFactoid(fName);
                    if (factoid != null)
                    {
                        System.out.println("found, issuing");
                        sendMessage(channel, runFactoid(factoid, channel, sender,
                            new String[0], new HashMap<String, String>()));
                    }
                }
            }
        }
    }
    
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
    
    protected void onKick(String channel, String kickerNick, String kickerLogin,
        String kickerHostname, String recipientNick, String reason)
    {
        if (recipientNick.equals(this.getName()))
            joinChannel(channel);
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
    
    protected void onMessage(String channel, String sender, String login,
        String hostname, String message)
    {
        Channel chan = storage.getChannel(channel);
        if (chan == null)
            return;
        String trigger = chan.getTrigger();
        if (trigger != null && message.startsWith(trigger))
        {
            try
            {
                runMessageCommand(channel, false, sender, hostname, message
                    .substring(trigger.length()));
            }
            catch (Exception e)
            {
                e.printStackTrace();
                throw new RuntimeException(e);
            }
        }
    }
    
    private void runMessageCommand(String channel, boolean pm, String sender,
        String hostname, String message)
    {
        String[] commandSplit = message.split(" ", 2);
        String command = commandSplit[0];
        String commandArguments = (commandSplit.length == 1 ? "" : commandSplit[1]);
        Command c = commands.get(command);
        if (c != null)
        {
            try
            {
                c.run(channel, pm, sender, hostname, commandArguments);
            }
            catch (Exception e)
            {
                if (e instanceof ResponseException)
                {
                    sendMessage(pm ? sender : channel, ((ResponseException) e)
                        .getMessage());
                }
                else
                {
                    e.printStackTrace();
                    sendMessage(pm ? sender : channel,
                        "An error occured while running the command " + command + ":");
                    StringWriter sw = new StringWriter();
                    e.printStackTrace(new PrintWriter(sw, true));
                    String[] eTokens = sw.toString().split("\n");
                    for (int i = 0; i < eTokens.length && i < 2; i++)
                    {
                        sendMessage(pm ? sender : channel, eTokens[i]);
                    }
                    if (eTokens.length > 2)
                        sendMessage(pm ? sender : channel, "...");
                    sendMessage(pm ? sender : channel,
                        "The full stack trace of the exception has been printed to stdout.");
                    sendMessage(pm ? sender : channel, "End of exception report.");
                }
            }
            return;
        }
        /*
         * If we get here, then the text isn't a command. We'll check to see if
         * it's a factoid.
         * 
         * Our first check will be for a channel-specific factoid.
         */
        if (channel != null)
        {
            Channel cn = storage.getChannel(channel);
            if (cn != null)
            {
                Factoid f = cn.getFactoid(command);
                if (f != null)
                {
                    sendMessage(channel, runFactoid(f, channel, sender,
                        commandArguments.split(" "), new HashMap<String, String>()));
                    return;
                }
            }
        }
        /*
         * Now we'll check for a global factoid.
         */
        Factoid f = storage.getFactoid(command);
        if (f != null)
        {
            sendMessage(channel, runFactoid(f, channel, sender, commandArguments
                .split(" "), new HashMap<String, String>()));
            return;
        }
        doInvalidCommand(pm, channel, sender);
    }
    
    private void doInvalidCommand(boolean pm, String channel, String sender)
    {
        if (channel != null)
            sendMessage(pm ? sender : channel, "Huh? (pm \"help\" for more info)");
        else
            sendMessage(pm ? sender : channel, "Huh? (pm \"help\" for more info)");
    }
    
    protected void onConnect()
    {
        for (Channel channel : storage.getChannels().isolate())
        {
            if (!channel.isSuspended())
            {
                System.out.println("joining " + channel.getName());
                joinChannel(channel.getName());
            }
        }
    }
    
    protected void onDisconnect()
    {
        new Thread()
        {
            public void run()
            {
                int attempts = 0;
                while (true)
                {
                    try
                    {
                        attempts++;
                        int time;
                        if (attempts < 5)
                            time = 1;
                        else if (attempts < 10)
                            time = 5;
                        else if (attempts < 20)
                            time = 15;
                        else if (attempts < 40)
                            time = 30;
                        else if (attempts < 70)
                            time = 60;
                        else if (attempts < 100)
                            time = 120;
                        else
                            time = 240;
                        Thread.sleep(time * 1000);
                        reconnect();
                    }
                    catch (Exception e)
                    {
                        e.printStackTrace();
                        continue;
                    }
                    return;
                }
            }
        }.start();
    }
    
    protected void onPrivateMessage(String sender, String login, String hostname,
        String message)
    {
        String channel = null;
        if (message.startsWith("#") && message.contains(" "))
        {
            channel = message.substring(0, message.indexOf(" "));
            message = message.substring(message.indexOf(" ") + 1);
        }
        try
        {
            runMessageCommand(channel, true, sender, hostname, message);
        }
        catch (Exception e)
        {
            e.printStackTrace();
            throw new RuntimeException(e);
        }
        
    }
    
    private void doHelp(String sender)
    {
        // TODO Auto-generated method stub
        
    }
    
    public static boolean isOp(String channel, String hostname)
    {
        if (isSuperop(hostname))
            return true;
        Channel c = storage.getChannel(channel);
        if (c == null)
            return false;
        return c.getOperator(hostname) != null;
    }
    
    public static boolean isSuperop(String hostname)
    {
        return storage.getOperator(hostname) != null;
    }
    
    public void verifyOp(String channel, String hostname)
    {
        if (!isOp(channel, hostname))
            throw new ResponseException(
                "You are not an op, so you don't have permission to run this command.");
    }
    
    public void verifySuperop(String hostname)
    {
        if (!isSuperop(hostname))
            throw new ResponseException(
                "You are not a superop, so you don't have permission to run this command.");
    }
}
