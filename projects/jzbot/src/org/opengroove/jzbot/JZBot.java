package org.opengroove.jzbot;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import net.sf.opengroove.common.proxystorage.ProxyStorage;

import org.jibble.pircbot.IrcException;
import org.jibble.pircbot.NickAlreadyInUseException;
import org.jibble.pircbot.PircBot;
import org.opengroove.jzbot.commands.CommandListCommand;
import org.opengroove.jzbot.commands.ConfigCommand;
import org.opengroove.jzbot.commands.FactoidCommand;
import org.opengroove.jzbot.commands.GoogleCommand;
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
import org.opengroove.jzbot.plugins.Command;
import org.opengroove.jzbot.plugins.CommandInvocationContext;
import org.opengroove.jzbot.plugins.NoSuchCommandException;
import org.opengroove.jzbot.storage.*;

/**
 * jzbot authenticates off of hostmask.
 */
public class JZBot extends PircBot
{
    private static ArrayList<Command> allCommands = new ArrayList<Command>();
    public static final JZBot bot = new JZBot();
    // numeric 320: is signed on as account
    private static ProxyStorage<Storage> proxyStorage;
    public static Storage storage;
    
    public static boolean isRunning;
    
    public static void main(String[] args) throws Throwable
    {
        proxyStorage = new ProxyStorage<Storage>(Storage.class, new File("storage/db"));
        storage = proxyStorage.getRoot();
        
    }
    
    /**
     * Gets the nickname for the specified user. If the url is a nickname url,
     * then the nickname is simply taken from the url. If the url is an
     * authenticated url, then the corresponding protocol is asked for the
     * user's nickname.
     * 
     * @param user
     * @return
     */
    public static String getNickname(URI user)
    {
        return null;
    }
    
    /**
     * Gets the authenticated name for the specified user. If the user is an
     * authenticated url, then the authenticated name is simply taken from the
     * url. If the url is a nickname url, then the protocol is asked for the
     * user's authenticated name.<br/>
     * <br/>
     * 
     * Due to the fact that nicknames can change frequently while authenticated
     * names generally won't, this should be called as soon after receiving a
     * nickname as possible, to avoid the wrong authenticated name being
     * obtained.<br/>
     * <br/>
     * 
     * It's possible that the user's authenticated name and the user's nickname
     * are the same.
     * 
     * @param user
     * @return The user's authenticated name, or null if their protocol reports
     *         that the user is not authenticated
     */
    public static String getAuthname(URI user)
    {
        return null;
    }
    
    /**
     * Converts the specified nickname url to an authenticated name url, or null
     * if the user specified is not authenticated. If the url is already an
     * authenticated url, then it is returned as-is.
     * 
     * @param user
     * @return
     */
    public static URI toAuthForm(URI user)
    {
        return null;
    }
    
    public static URI toNickForm(URI user)
    {
        return null;
    }
    
    /**
     * Checks to see if the users specified represent the same user. This
     * involves converting both of them to a nickname form, removing the
     * fragment, if any, and then comparing the urls.
     * 
     * @param user1
     * @param user2
     * @return
     */
    public static boolean usersEqual(URI user1, URI user2)
    {
        URI u1n = toNickForm(user1);
        URI u2n = toNickForm(user2);
        u1n = uriRemoveFragment(u1n);
        u2n = uriRemoveFragment(u2n);
        return u1n.equals(u2n);
    }
    
    private static URI uriRemoveFragment(URI uri)
    {
        try
        {
            return new URI(uri.getScheme(), uri.getUserInfo(), uri.getHost(), uri
                .getPort(), uri.getPath(), uri.getQuery(), null);
        }
        catch (URISyntaxException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
            throw new RuntimeException(e.getClass().getName() + ": " + e.getMessage(),
                e);
        }
    }
    
    /**
     * Executes the specified command, if one can process it. The command is
     * executed in the context specified. The context is <b>not</b> finished
     * after execution, so the caller should make sure and finish it.
     * 
     * @param context
     * @param command
     * @param arguments
     */
    public static void executeCommandToContext(CommandInvocationContext context,
        String command, String arguments, URI source, URI user)
    {
        Command commandToRun = null;
        for (Command c : allCommands)
        {
            if (c.canProcess(command, arguments, source, user))
            {
                commandToRun = c;
                break;
            }
        }
        if (commandToRun == null)
            throw new NoSuchCommandException("No such command: " + command);
        commandToRun.process(command, arguments, context);
    }
    
    public static void executeCommandFromSource()
    {
        
    }
}
