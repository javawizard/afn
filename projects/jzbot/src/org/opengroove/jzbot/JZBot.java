package org.opengroove.jzbot;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;

import net.sf.opengroove.common.proxystorage.ProxyStorage;

import org.jibble.pircbot.IrcException;
import org.jibble.pircbot.NickAlreadyInUseException;
import org.jibble.pircbot.PircBot;
import org.opengroove.jzbot.commands.RouletteCommand;
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
    
    public static void main(String[] args) throws Throwable
    {
        bot.start();
    }
    
    private static void loadCommands()
    {
        loadCommand(new RouletteCommand());
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
        if (chan.getJoinFactoid() != null)
        {
            Factoid factoid = storage.getFactoid(chan.getJoinFactoid());
            if (factoid != null)
                runFactoid(factoid, channel, sender);
        }
    }
    
    /**
     * Runs the factoid specified, outputting to the channel specified.
     * 
     * @param factoid
     *            The factoid to run
     * @param channel
     *            The channel to send to
     * @param sender
     *            The sender of the factoid request
     */
    private void runFactoid(Factoid factoid, String channel, String sender)
    {
        // TODO Auto-generated method stub
        
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
            runMessageCommand(channel, sender, hostname, message.substring(trigger
                .length()));
        }
    }
    
    private void runMessageCommand(String channel, String sender, String hostname,
        String message)
    {
        String[] commandSplit = message.split(" ", 2);
        String command = commandSplit[0];
        String commandArguments = (commandSplit.length == 1 ? "" : commandSplit[1]);
        Command c = commands.get(command);
        if (c != null)
        {
            c.run(commandArguments);
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
                    runFactoid(f, channel, sender);
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
            runFactoid(f, channel, sender);
            return;
        }
        doInvalidCommand(channel, sender);
    }
    
    private void doInvalidCommand(String channel, String sender)
    {
        if (channel != null)
            sendMessage(channel, "Huh? (pm \"help\" for more info)");
        else
            sendMessage(sender, "Huh? (pm \"help\" for more info)");
    }
    
    protected void onConnect()
    {
        for (Channel channel : storage.getChannels().isolate())
        {
            joinChannel(channel.getName());
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
        String channel = "";
        if (message.startsWith("#") && message.contains(" "))
        {
            channel = message.substring(0, message.indexOf(" "));
            message = message.substring(message.indexOf(" ") + 1);
        }
        runMessageCommand(channel, sender, hostname, message);
    }
    
    private void doHelp(String sender)
    {
        // TODO Auto-generated method stub
        
    }
}
