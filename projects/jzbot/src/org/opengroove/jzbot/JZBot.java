package org.opengroove.jzbot;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.sf.opengroove.common.proxystorage.ProxyStorage;

import org.jibble.pircbot.IrcException;
import org.jibble.pircbot.NickAlreadyInUseException;
import org.jibble.pircbot.PircBot;
import org.opengroove.jzbot.com.Protocol;
import org.opengroove.jzbot.com.ProtocolContext;
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
import org.opengroove.jzbot.plugins.InvalidProtocolException;
import org.opengroove.jzbot.plugins.InvalidURIException;
import org.opengroove.jzbot.plugins.Message;
import org.opengroove.jzbot.plugins.NoSuchCommandException;
import org.opengroove.jzbot.plugins.TargetedMessage;
import org.opengroove.jzbot.storage.*;

/**
 * jzbot authenticates off of hostmask.
 */
public class JZBot extends PircBot
{
    private static List<Command> allCommands = new ArrayList<Command>();
    private static Map<String, Protocol> allProtocols = new HashMap<String, Protocol>();
    public static final JZBot bot = new JZBot();
    // numeric 320: is signed on as account
    private static ProxyStorage<Storage> proxyStorage;
    public static Storage storage;
    
    public static boolean isRunning;
    
    public static void main(String[] args) throws Throwable
    {
        proxyStorage = new ProxyStorage<Storage>(Storage.class, new File("storage/db"));
        storage = proxyStorage.getRoot();
        Defaults.installDefaultProtocols();
        Defaults.installDefaultCommands();
        
    }
    
    public static void installCommand(Command command)
    {
        command.init();
        allCommands.add(command);
    }
    
    /**
     * Creates a context for the specified protocol, initializes the protocol,
     * and registers it to JZBot. It takes care of everything, so you can just
     * do something like <tt>JZBot.installProtocol(new IRCProtocol());</tt>, and
     * JZBot will take care of everything else.
     * 
     * @param protocol
     */
    public static void installProtocol(Protocol protocol)
    {
        protocol.init(new ProtocolContext(protocol.getName()));
        allProtocols.put(protocol.getName(), protocol);
    }
    
    public static Protocol getProtocol(String name)
    {
        return allProtocols.get(name);
    }
    
    /**
     * Same as getProtocol, but instead of returning null, this method throws an
     * exception.
     * 
     * @param name
     * @return
     */
    public static Protocol getCheckedProtocol(String name)
    {
        Protocol p = getProtocol(name);
        if (p == null)
            throw new InvalidProtocolException("Protocol " + name + " does not exist");
        return p;
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
    
    public static void validateUserUri(URI uri)
    {
        /*
         * User uris don't have a path and have a query string that starts with
         * "n=" or "a=".
         */
        boolean validPath = uri.getPath().equals("");
        boolean validQuery =
            uri.getQuery() != null
                && (uri.getQuery().startsWith("a=") || uri.getQuery().startsWith("n="));
        if (!(validPath && validQuery))
            throw new InvalidURIException(uri.toString() + " is not a valid user uri");
    }
    
    public static void validateRoomUri(URI uri)
    {
        /*
         * Room uris have any sort of path and an empty query string. The path
         * can be empty (such as a bzflag server which is its own room).
         */
        if (!(uri.getQuery() == null))
            throw new InvalidURIException(uri.toString() + " is not a valid room uri");
    }
    
    public void validateServerUri(URI uri)
    {
        /*
         * Server uris have an empty query and an empty path.
         */
        boolean validQuery = uri.getQuery() == null;
        if (!(validQuery && uri.getPath().equals("")))
            throw new InvalidURIException(uri.toString() + " is not a valid server uri");
    }
    
    /**
     * Extracts a server uri from a room or user uri.
     * 
     * @param uri
     */
    public static URI extractServerUri(URI uri)
    {
        try
        {
            return new URI(uri.getScheme(), uri.getUserInfo(), uri.getHost(), uri
                .getPort(), "", null, null);
        }
        catch (URISyntaxException e)
        {
            e.printStackTrace();
            throw new RuntimeException(e.getClass().getName() + ": " + e.getMessage(),
                e);
        }
    }
    
    public static String getDisplayName(URI target)
    {
        validateUserUri(target);
        Protocol p = getCheckedProtocol(target.getScheme());
        return p.getDisplayName(target);
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
        validateUserUri(user);
        if (user.getQuery().startsWith("a="))
            return user.getQuery().substring(2);
        return getCheckedProtocol(user.getScheme()).getAuthenticatedUrl(user)
            .getQuery().substring(2);
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
        validateUserUri(user);
        if (user.getQuery().startsWith("a="))
            return user;
        return getCheckedProtocol(user.getScheme()).getAuthenticatedUrl(user);
    }
    
    public static URI toNickForm(URI user)
    {
        validateUserUri(user);
        if (user.getQuery().startsWith("n="))
            return user;
        return getCheckedProtocol(user.getScheme()).getNicknameUrl(user);
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
    
    /**
     * Executes the specified command directly from a source, sending to-source
     * messages and other messages on their way after the command has finished
     * executing.<br/>
     * <br/>
     * 
     * This should not be called from a command when it is trying to include
     * another command.<br/>
     * <br/>
     * 
     * This throws an exception if a problem happens while trying to run the
     * command. Whatever calls this should handle such exceptions by, for
     * example, pastebinning the message and sending the paste as a response.
     * 
     * @param source
     *            The source of the command, which is either a room or
     *            <tt>user</tt>
     * @param user
     *            The user that initiated the command
     * @param command
     *            The command itself
     * @param arguments
     *            The command's arguments
     */
    public static void executeCommandFromSource(URI room, URI user, String command,
        String arguments, Message original)
    {
        URI source;
        if (room == null)
            source = user;
        else
            source = room;
        CommandInvocationContext context = new CommandInvocationContext(source, user);
        executeCommandToContext(context, command, arguments, source, user);
        context.finish();
        Message[] toSourceMessages = context.getToSourceMessages();
        TargetedMessage[] otherMessages = context.getOtherMessages();
        /*
         * TODO: This doesn't preserve ordering of to-source messages mixed with
         * other messages. Consider storing them as one array in the context,
         * but with some sort of flag set so that they're different, like have a
         * wrapper class or something.
         * 
         * For now, we'll send other messages first, and then to-source
         * messages. And we'll send each other message in a separate call to the
         * protocol, but we'll send all to-source messages in the same call.
         */
        for (TargetedMessage m : otherMessages)
        {
            Protocol p = getCheckedProtocol(m.getTarget().getScheme());
            p.sendMessage(m.getTarget(), new Message[] { m },
                new Message[] { original });
        }
        /*
         * Now we'll send the to-source messages.
         */
        Protocol p = getCheckedProtocol(source.getScheme());
        Message[] sourceMessageSources = new Message[toSourceMessages.length];
        Arrays.fill(sourceMessageSources, original);
        p.sendMessage(source, toSourceMessages, sourceMessageSources);
    }
    
    public static synchronized void fromProtocolJoined(String protocolName, URI room,
        URI user)
    {
        // TODO Auto-generated method stub
        
    }
    
    public static synchronized void fromProtocolLeft(String protocolName, URI room,
        URI user)
    {
        // TODO Auto-generated method stub
        
    }
    
    public static synchronized void fromProtocolMessage(String protocolName, URI room,
        URI user, Message[] messages)
    {
        /*
         * For each message, we'll try to parse it. If it is a prvmsg, then we
         * parse and run as a command. If it is a room message, then we fetch
         * the room and check to see if the command starts with the room's
         * trigger, and if it does, then we parse and run it as a command.
         */
        boolean roomMessage = room != null;
        Room storedRoom = null;
        if (roomMessage)
        {
            Server server = storage.getServer(extractServerUri(room));
            if (server == null)
                throw new RuntimeException("Invalid server ref from room " + room
                    + " as " + extractServerUri(room));
            storedRoom = server.getRoom(room);
            if (server == null)
                throw new RuntimeException("Invalid room ref " + room);
        }
        String trigger = "";
        if (roomMessage)
            trigger = storedRoom.getTrigger();
        for (Message message : messages)
        {
            String text = message.getMessage();
            if (text.trim().equals(""))
                continue;
            String[] textSplit = text.split(" ", 2);
            String command = textSplit[0];
            String arguments = textSplit.length > 1 ? textSplit[1] : "";
            if (command.startsWith(trigger))
                executeCommandFromSource(room, user, command
                    .substring(trigger.length()), arguments, message);
        }
    }
    
    public static synchronized void fromProtocolExtendedEvent(String protocolName,
        String event, URI user, URI room, String[] arguments)
    {
        // TODO Auto-generated method stub
        
    }
    
    public static synchronized void fromProtocolRoomOpRemoved(URI room, URI user,
        URI from)
    {
        // TODO Auto-generated method stub
        
    }
    
    public static synchronized void fromProtocolKicked(URI room, URI user, URI from)
    {
        // TODO Auto-generated method stub
        
    }
}
