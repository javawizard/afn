package org.opengroove.jzbot;

import java.io.File;
import java.io.IOException;

import net.sf.opengroove.common.proxystorage.ProxyStorage;

import org.jibble.pircbot.IrcException;
import org.jibble.pircbot.NickAlreadyInUseException;
import org.jibble.pircbot.PircBot;
import org.opengroove.jzbot.storage.*;

/**
 * jzbot authenticates off of hostmask.
 */
public class JZBot extends PircBot
{
    public static final JZBot bot = new JZBot();
    // numeric 320: is signed on as account
    private static ProxyStorage<Storage> proxyStorage;
    public static Storage storage;
    
    public static Config config;
    
    public static void main(String[] args) throws Throwable
    {
        bot.start();
    }
    
    private void start() throws Throwable
    {
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
        runMessageCommand(channel, sender, hostname, message);
    }
    
    private void doHelp(String sender)
    {
        // TODO Auto-generated method stub
        
    }
}
