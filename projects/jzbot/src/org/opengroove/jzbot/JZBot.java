package org.opengroove.jzbot;

import java.io.File;
import java.io.IOException;

import net.sf.opengroove.common.proxystorage.ProxyStorage;

import org.jibble.pircbot.IrcException;
import org.jibble.pircbot.NickAlreadyInUseException;
import org.jibble.pircbot.PircBot;
import org.opengroove.jzbot.storage.Config;
import org.opengroove.jzbot.storage.Storage;

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
}
