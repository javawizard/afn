package jw.jzbot;

/*
 * TODO: this file really needs to be split into multiple files at some point. 
 * It's 3149 lines long as of the writing of this comment, and that's a bit 
 * larger than is practical.
 */

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.RandomAccessFile;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Array;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.net.URI;
import java.net.URL;
import java.nio.charset.Charset;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.plaf.basic.BasicInternalFrameTitlePane.SystemMenuBar;

import jw.jzbot.commands.CommandListCommand;
import jw.jzbot.commands.ConfigCommand;
import jw.jzbot.commands.CrosstalkCommand;
import jw.jzbot.commands.ExecCommand;
import jw.jzbot.commands.HelpCommand;
import jw.jzbot.commands.JoinCommand;
import jw.jzbot.commands.LeaveCommand;
import jw.jzbot.commands.ListChannelsCommand;
import jw.jzbot.commands.MMCommand;
import jw.jzbot.commands.PluginCommand;
import jw.jzbot.commands.RedefineCommand;
import jw.jzbot.commands.RegexCommand;
import jw.jzbot.commands.RestartCommand;
import jw.jzbot.commands.ScopeCommand;
import jw.jzbot.commands.ServerCommand;
import jw.jzbot.commands.ShutdownCommand;
import jw.jzbot.commands.StatusCommand;
import jw.jzbot.commands.SuperopCommand;
import jw.jzbot.commands.SwitchnickCommand;
import jw.jzbot.commands.TriggerCommand;
import jw.jzbot.commands.UpdateCommand;
import jw.jzbot.commands.factoid.FactoidCommand;
import jw.jzbot.configuration.Configuration;
import jw.jzbot.configuration.VarListener;
import jw.jzbot.eval.CaltechEvaluator;
import jw.jzbot.eval.JEvalEvaluator;
import jw.jzbot.eval.JepliteEvaluator;
import jw.jzbot.events.Notify;
import jw.jzbot.fact.ArgumentList;
import jw.jzbot.fact.FactContext;
import jw.jzbot.fact.FactParser;
import jw.jzbot.fact.FactQuota;
import jw.jzbot.fact.ast.FactEntity;
import jw.jzbot.fact.exceptions.FactoidException;
import jw.jzbot.fact.output.NullSink;
import jw.jzbot.fact.output.StringSink;
import jw.jzbot.help.DefaultHelpProvider;
import jw.jzbot.help.FunctionHelpProvider;
import jw.jzbot.help.HelpSystem;
import jw.jzbot.help.PropsHelpProvider;
import jw.jzbot.help.XMLHelpProvider;
import jw.jzbot.pastebin.DefaultPastebinProviders;
import jw.jzbot.pastebin.PastebinUtils;
import jw.jzbot.pastebin.PastebinProvider.Feature;
import jw.jzbot.plugins.PluginSystem;
import jw.jzbot.protocols.Connection;
import jw.jzbot.protocols.ProtocolManager;
import jw.jzbot.protocols.bzflag.BZFlagProtocol;
import jw.jzbot.protocols.fb.FacebookProtocol;
import jw.jzbot.protocols.imap.ImapProtocol;
import jw.jzbot.protocols.irc.IrcProtocol;
import jw.jzbot.protocols.xmpp.XmppProtocol;
import jw.jzbot.scope.Messenger;
import jw.jzbot.scope.Scope;
import jw.jzbot.scope.ScopeLevel;
import jw.jzbot.scope.ChannelScope;
import jw.jzbot.scope.UserMessenger;
import jw.jzbot.storage.*;
import jw.jzbot.utils.Utils;
import jw.jzbot.utils.Pastebin;

import net.sf.opengroove.common.proxystorage.ProxyObject;
import net.sf.opengroove.common.proxystorage.ProxyStorage;
import net.sf.opengroove.common.proxystorage.StoredList;
import net.sf.opengroove.common.utils.StringUtils;
import net.sf.opengroove.common.utils.StringUtils.ToString;

import org.cheffo.jeplite.JEP;
import org.jibble.pircbot.IrcException;
import org.jibble.pircbot.NickAlreadyInUseException;
import org.jibble.pircbot.PircBot;
import org.jibble.pircbot.User;
import org.python.core.Options;
import org.python.util.PythonInterpreter;

import sun.misc.Unsafe;

/**
 * jzbot authenticates off of hostmask.
 */
public class JZBot
{
    // public static Connection bot;
    public static Map<String, ConnectionContext> connectionMap =
            new HashMap<String, ConnectionContext>();
    
    public static File restartFile = new File("storage/restart");
    
    public static File logsFolder = new File("storage/logs");
    
    public static Map<String, String> globalVariables = Collections
            .synchronizedMap(new HashMap<String, String>());
    
    public static Map<Integer, HttpServer> httpServers = new HashMap<Integer, HttpServer>();
    
    public static volatile int notificationSequence = 0;
    
    public static Map<String, String> pmUserScopeMap = new HashMap<String, String>();
    
    public static Map<String, Long> pmUserScopeTimes = new HashMap<String, Long>();
    
    public static Object pmUserScopeLock = new Object();
    /**
     * When a user sets their pm scope with the ~scope command, this is the number of
     * minutes that the scope will stay before being reset to the server scope.
     */
    public static final long PM_USER_SCOPE_TIMEOUT = 1000 * 60 * 10;// 10 minutes
    
    public static Thread pmUserScopeTimeoutThread = new Thread("pm-user-scope-timeout")
    {
        public void run()
        {
            while (isRunning)
            {
                Utils.sleep(60 * 1000);
                try
                {
                    synchronized (pmUserScopeLock)
                    {
                        /*
                         * First, we'll remove all entries that don't have a time
                         * associated with them. This generally won't happen, but I'm
                         * paranoid.
                         */
                        pmUserScopeMap.keySet().retainAll(pmUserScopeTimes.keySet());
                        /*
                         * Now we check over all of the entries that do have a time entry,
                         * and remove them if they're too old.
                         */
                        for (String key : new ArrayList<String>(pmUserScopeMap.keySet()))
                        {
                            Long v = pmUserScopeTimes.get(key);
                            if (v == null
                                || (v + PM_USER_SCOPE_TIMEOUT) < System.currentTimeMillis())
                                pmUserScopeMap.remove(key);
                        }
                        /*
                         * Last, we'll remove all time keys where we don't have
                         */
                        pmUserScopeTimes.keySet().retainAll(pmUserScopeMap.keySet());
                    }
                }
                catch (Exception e)
                {
                    e.printStackTrace();
                }
            }
        }
    };
    
    public static BlockingQueue<LogEvent> logQueue;
    
    public static final Object logQueueLock = new Object();
    // TODO: consider changing this to 90
    private static final long CONNECTION_CYCLE_TIMEOUT = 120;
    
    public static Thread notificationThread = new Thread("factoid-cron-thread")
    {
        public void run()
        {
            while (true)
            {
                try
                {
                    Thread.sleep(1000 * 60 * 5);
                    notificationSequence += 1;
                    notificationSequence %= 12;
                    sendNotificationToAll("fiveminutes");
                    if ((notificationSequence % 2) == 0)
                        sendNotificationToAll("tenminutes");
                    if ((notificationSequence % 6) == 0)
                        sendNotificationToAll("halfhour");
                    if ((notificationSequence % 12) == 0)
                        sendNotificationToAll("hour");
                }
                catch (Exception e)
                {
                    e.printStackTrace();
                }
            }
        }
    };
    static
    {
        notificationThread.setDaemon(true);
        notificationThread.start();
    }
    
    public static void startHttpServer(int port, String factoid)
    {
        try
        {
            synchronized (httpServers)
            {
                if (httpServers.get(port) != null)
                    throw new RuntimeException(
                            "A server with that port has already been started, "
                                + "serving the factoid "
                                + httpServers.get(port).getFactoid());
                verifyStartServer(port);
                HttpServer server = new HttpServer(port, factoid);
                httpServers.put(port, server);
                server.startServer();
            }
        }
        catch (Exception e)
        {
            throw new FactoidException(
                    "Exception occured while starting an http server on port " + port
                        + " with factoid " + factoid, e);
        }
    }
    
    /**
     * Runs the specified notification first globally, then on all connected servers
     * (Read: not servers that are disconnected, deactivated, or no longer existent), then
     * on all channels within those servers.
     * 
     * @param name
     */
    protected static void sendNotificationToAll(String name)
    {
        try
        {
            System.out.println("Running reverse-cascade notification " + name);
            runNotificationFactoid(null, null, null, null, "", "", "", "_on" + name,
                    new String[0], true, false);
            synchronized (connectionCycleLock)
            {
                System.out.println("Reverse-cascade notification locked "
                    + "on connection cycle lock");
                for (ConnectionContext context : connectionMap.values())
                {
                    if (!context.getConnection().isConnected())
                        continue;
                    System.out.println("Running reverse-cascade notification " + name
                        + " at @" + context.getServerName());
                    runNotificationFactoid(context.getServerName(),
                            context.getDatastoreServer(), null, null, context
                                    .getConnection().getNick(), null, null, "_on" + name,
                            new String[0], true, false);
                    for (Channel channel : context.getDatastoreServer().getChannels()
                            .isolate())
                    {
                        System.out.println("Running reverse-cascade notification " + name
                            + " at @" + context.getServerName() + channel.getName());
                        runNotificationFactoid(context.getServerName(),
                                context.getDatastoreServer(), channel.getName(), channel,
                                context.getConnection().getNick(), null, null,
                                "_on" + name, new String[0], true, false);
                    }
                }
            }
            System.out.println("Reverse-cascade notification unlocked");
        }
        catch (Throwable e)
        {
            System.out.println("Reverse-cascade notification " + name + " failed");
            e.printStackTrace();
            try
            {
                sendMessageToTarget(
                        Configuration.getText(null, "primary"),
                        "Global notification failure for " + name + ": "
                            + PastebinUtils.pastebinStack(e));
            }
            catch (Throwable e2)
            {
                e2.printStackTrace();
            }
        }
        finally
        {
            System.out.println("Finished reverse-cascade notification " + name + ".");
        }
    }
    
    /**
     * Sends a message to the specified target, which is a canonical channel name. If the
     * specified server is not connected or if the specified channel is not joined, this
     * message is silently discarded.<br/>
     * <br/>
     * 
     * This <b>must not</b> be called from any logging code, as this method will invoke
     * the channel logger to log that a message was sent.
     * 
     * @param target
     *            The target to send to, which can be null. If it's null, the message will
     *            be silently discarded.
     * @param message
     *            The message to send
     */
    public static void sendMessageToTarget(String target, String message)
    {
        if (target == null)
            return;
        String serverName;
        String channelName;
        try
        {
            serverName = extractServerName(target);
            channelName = extractChannelName(target);
        }
        catch (Exception e)
        {
            /*
             * This will happen if the target is malformed. We'll just ignore it for now.
             */
            e.printStackTrace();
            return;
        }
        ConnectionWrapper wrapper = getConnection(serverName);
        if (wrapper == null)
            return;
        ConnectionContext context = wrapper.getContext();
        Connection connection = context.getConnection();
        if (!connection.isConnected())
            return;
        wrapper.sendMessage(channelName, message);
    }
    
    public static String extractChannelName(String target)
    {
        if (target.startsWith("#"))
            return target;
        if (target.startsWith("@") && target.contains("#"))
            return target.substring(target.indexOf('#'));
        throw new IllegalArgumentException("The target \"" + target
            + "\" does not contain a valid channel.");
    }
    
    public static String extractServerName(String target)
    {
        if (target.startsWith("@") && target.contains("#"))
            return target.substring(1, target.indexOf('#'));
        if (target.startsWith("@"))
            return target.substring(1);
        throw new IllegalArgumentException("The target \"" + target
            + "\" does not contain a valid server name.");
    }
    
    public static String extractRelativeServer(String target, Scope scope)
    {
        if (target.startsWith("@") && target.contains("#"))
            return target.substring(1, target.indexOf('#'));
        if (target.startsWith("@"))
            return target.substring(1);
        if (scope != null)
            return scope.getServerName();
        return null;
    }
    
    public static String extractRelativeChannel(String target, Scope scope)
    {
        if (target.startsWith("#"))
            return target;
        if (target.startsWith("@") && target.contains("#"))
            return target.substring(target.indexOf('#'));
        if (scope != null && !target.startsWith("@"))
            /*
             * The check for "@" is included so that we don't get the channel we're scoped
             * to on an entirely different server.
             */
            return scope.getChannelName();
        return null;
    }
    
    /**
     * Gets the connection object for the specified server, if it is currently connected.
     * If it is not currently connected, null is returned.
     * 
     * @param serverName
     * @return
     */
    public static ConnectionWrapper getConnection(String serverName)
    {
        ConnectionContext con = connectionMap.get(serverName);
        if (con == null)
            return null;
        if (!con.getConnection().isConnected())
            return null;
        // FIXME: add caching of the connection wrapper
        return new ConnectionWrapper(con);
    }
    
    /**
     * Extracts the server name from the target specified (which could be
     * "@server#channel", for example), then gets and returns the connection for that
     * server. If the target does not specify a server, then the connection for the server
     * <tt>scope</tt> is currently using is returned instead. If <tt>scope</tt> doesn't
     * contain a server and <tt>target</tt> doesn't contain one either, an exception is
     * thrown.
     * 
     * @param target
     * @param scope
     * @return
     */
    public static ConnectionWrapper checkedGetExtractedConnection(String target, Scope scope)
    {
        String serverName = extractRelativeServer(target, scope);
        if (serverName == null)
            throw new FactoidException("The target \"" + target
                + "\" was expected to contain a server name, but it "
                + "did not and the current scope doesn't contain "
                + "a server name either. Consider wrapping this "
                + "function call with a call to the {scope} "
                + "function to add a server to the current scope, "
                + "or just specify a server in the target string " + "that you're using.");
        ConnectionWrapper con = getConnection(serverName);
        if (con == null)
            throw new FactoidException("There isn't a connection for the server name "
                + serverName
                + ". This probably means that the server is currently disconnected.");
        return con;
    }
    
    public static ConnectionContext getRealConnection(String serverName)
    {
        ConnectionContext con = connectionMap.get(serverName);
        if (con == null)
            return null;
        if (!con.getConnection().isConnected())
            return null;
        return con;
    }
    
    public static class ConnectionCycleThread extends Thread
    {
        public void run()
        {
            while (isRunning)
            {
                try
                {
                    doSingleConnectionCycle();
                }
                catch (Exception e)
                {
                    e.printStackTrace();
                    // TODO: consider making these errors available to the bot's users?
                }
            }
        }
    }
    
    private static ConnectionCycleThread connectionCycleThread;
    
    private static BlockingQueue<Object> connectionCycleQueue =
            new LinkedBlockingQueue<Object>(500);
    
    public static Map<String, Throwable> connectionLastErrorMap =
            new HashMap<String, Throwable>();
    
    public static final Object connectionCycleLock = new Object();
    
    public static void startConnectionCycleThread()
    {
        connectionCycleThread = new ConnectionCycleThread();
        connectionCycleThread.setDaemon(true);
        connectionCycleThread.setPriority(3);
        connectionCycleThread.start();
    }
    
    /**
     * This is called about once every 2 minutes. It performs the main connection cycle.
     * It essentially connects the bot to servers that it's supposed to be connected to,
     * and disconnects the bot from servers that it's not supposed to be connected to.
     * 
     * @throws InterruptedException
     */
    public static void doSingleConnectionCycle() throws InterruptedException
    {
        connectionCycleQueue.poll(CONNECTION_CYCLE_TIMEOUT, TimeUnit.SECONDS);
        connectionCycleQueue.clear();
        // We'll wait a bit, mostly for the heck of it
        Thread.sleep(2000);
        System.out.println("Connection cycle");
        if (!isRunning)
            return;
        /*
         * This is used to track threads spawned by the current step
         */
        ArrayList<Thread> currentStepThreads = new ArrayList<Thread>();
        /*
         * First step: create a connection object for all servers in the list
         */
        synchronized (connectionCycleLock)
        {
            System.out.println("Synchronized on connection cycle");
            if (!isRunning)
                return;
            System.out.println("Creating connection objects for all servers...");
            for (Server server : sortByPriority(storage.getServers().isolate()))
            {
                if (!isRunning)
                    return;
                String serverName = server.getName();
                System.out.println("Checking to see if server " + serverName
                    + " has a connection object");
                if (connectionMap.get(serverName) == null)
                {
                    /*
                     * This server doesn't have a corresponding connection. Let's create
                     * one for it.
                     */
                    System.out.println("Building connection for server " + serverName
                        + "...");
                    ConnectionContext context = new ConnectionContext();
                    context.setServerName(serverName);
                    context.setDatastoreServer(server);
                    System.out.println("Instantiating connection instance...");
                    Connection c = ProtocolManager.createConnection(server.getProtocol());
                    context.setConnection(c);
                    System.out.println("Initializing connection...");
                    c.init(context);
                    System.out.println("Registering connection...");
                    connectionMap.put(server.getName(), context);
                    System.out.println("Connection built successfully.");
                }
            }
        }
        /*
         * Second step: iterate through all connection objects and check to see if they
         * are disconnected. If they are, check to see if their database server object is
         * active. If it is, attempt to connect the connection. However, if the connection
         * has been connected at least once before (as determined by the connection
         * context), then the connection should be discarded and the connection cycle
         * thread notified. This will cause this whole method to run again, and that time
         * through it will create a new connection and connect it. This ensures that a
         * given connection object is never re-used.
         * 
         * We're going to start one thread per connection and then join all the threads so
         * that one server that's being slow to connect doesn't stall the rest of them
         * from connecting.
         * 
         * TODO: figure out a more elegant way to handle making sure connections are never
         * re-used. That idea was hacked into the system at the last minute, and I didn't
         * do a very good job at it.
         */
        for (ConnectionContext context : sortByPriority(connectionMap.values()))
        {
            if (!context.getConnection().isConnected())
            {
                if (context.getDatastoreServer().isActive())
                {
                    if (context.hasConnected())
                    {
                        context.markDiscardNeeded();
                        notifyConnectionCycleThread();
                    }
                    else
                    {
                        currentStepThreads.add(connectionCycleStartConnectThread(context));
                    }
                }
            }
        }
        joinAllAndClear(currentStepThreads);
        /*
         * Third step: disconnect all connections if they are connected but their server
         * object is inactive, their server object is no longer present in the server
         * objects list, or the connection has been marked as needing to be discarded.
         * We're going to do this in separate threads for the same reason that we did the
         * previous step in separate threads.
         * 
         * TODO: change it so that it's done in separate threads as noted
         */
        synchronized (connectionCycleLock)
        {
            for (ConnectionContext context : sortByPriority(connectionMap.values()))
            {
                if (context.getConnection().isConnected())
                {
                    if ((!context.getDatastoreServer().isActive())
                        || !storage.getServers().contains(context.getDatastoreServer())
                        || context.discardNeeded())
                    // if the server is not active or the server is no longer in the list
                    {
                        System.out.println("Disconnecting from server "
                            + context.getServerName());
                        context.getConnection().disconnect(
                                "Disconnecting from this server... " + PART_MESSAGE);
                        System.out.println("Disconnected.");
                    }
                }
            }
        }
        /*
         * Fourth step: find all connection objects whose server objects are not in the
         * list anymore, or that are marked as needing to be discarded, and delete them.
         */
        synchronized (connectionCycleLock)
        {
            for (ConnectionContext context : new ArrayList<ConnectionContext>(
                    connectionMap.values()))
            {
                if ((!storage.getServers().contains(context.getDatastoreServer()))
                    || context.discardNeeded())
                {
                    System.out.println("Discarding connection for server "
                        + context.getServerName());
                    context.getConnection().discard();
                    System.out.println("Unregistering connection...");
                    connectionMap.remove(context.getDatastoreServer().getName());
                    System.out.println("Connection discarded successfully.");
                }
            }
        }
        /*
         * ...and we're done!
         */
    }
    
    private static void joinAllAndClear(ArrayList<Thread> threads)
    {
        for (Thread thread : threads)
        {
            try
            {
                thread.join();
            }
            catch (InterruptedException e)
            {
                e.printStackTrace();
            }
        }
        threads.clear();
    }
    
    private static Thread connectionCycleStartConnectThread(final ConnectionContext context)
    {
        Thread thread =
                new Thread("connection-cycle-server-connect-" + context.getServerName())
                {
                    public void run()
                    {
                        try
                        {
                            System.out.println("Running pre-connect actions for server "
                                + context.getServerName());
                            runPreConnectActions(context);
                            System.out.println("Connecting to server "
                                + context.getServerName());
                            context.getConnection().connect();
                            context.markConnected();
                            System.out.println("Connection established.");
                            connectionLastErrorMap.remove(context.getServerName());
                        }
                        catch (Exception e)
                        {
                            e.printStackTrace();
                            connectionLastErrorMap.put(context.getServerName(), e);
                        }
                    }
                };
        thread.start();
        return thread;
    }
    
    private static List<ConnectionContext> sortByPriority(
            Collection<ConnectionContext> values)
    {
        ArrayList<ConnectionContext> list = new ArrayList<ConnectionContext>(values);
        long start = System.currentTimeMillis();
        Collections.sort(list, new Comparator<ConnectionContext>()
        {
            
            @Override
            public int compare(ConnectionContext first, ConnectionContext second)
            {
                if (first.getDatastoreServer().getPriority() > second.getDatastoreServer()
                        .getPriority())
                    return -1;
                else if (second.getDatastoreServer().getPriority() > first
                        .getDatastoreServer().getPriority())
                    return 1;
                return 0;
            }
        });
        System.out.println("Sorted server context list in "
            + (System.currentTimeMillis() - start) + " ms");
        return list;
    }
    
    private static List<Server> sortByPriority(ArrayList<Server> list)
    {
        long start = System.currentTimeMillis();
        final Map<Long, Integer> priorityCache = new HashMap<Long, Integer>();
        for (Server server : list)
            priorityCache.put(((ProxyObject) server).getProxyStorageId(),
                    server.getPriority());
        Collections.sort(list, new Comparator<Server>()
        {
            
            @Override
            public int compare(Server first, Server second)
            {
                ProxyObject o1 = (ProxyObject) first;
                ProxyObject o2 = (ProxyObject) second;
                if (priorityCache.get(o1.getProxyStorageId()) > priorityCache.get(o2
                        .getProxyStorageId()))
                    return -1;
                else if (priorityCache.get(o2.getProxyStorageId()) > priorityCache.get(o1
                        .getProxyStorageId()))
                    return 1;
                return 0;
            }
        });
        System.out.println("Sorted server list in " + (System.currentTimeMillis() - start)
            + " ms");
        return list;
    }
    
    public static void notifyConnectionCycleThread()
    {
        connectionCycleQueue.offer(new Object());
    }
    
    private static void runPreConnectActions(ConnectionContext context)
    {
        context.getConnection().setMessageDelay(Configuration.getInt(null, "delay"));
        // TODO: might want to change this to an onVersion method in IrcProtocol itself,
        // and have it retrievable from the context
        context.getConnection().setVersion("JZBot -- http://jzbot.googlecode.com");
        try
        {
            /*
             * We might want to make this configurable via the configuration system on a
             * per-server basis
             */
            context.getConnection().setEncoding("UTF-8");
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }
    
    public static void stopHttpServer(int port)
    {
        try
        {
            synchronized (httpServers)
            {
                HttpServer server = httpServers.get(port);
                if (server == null)
                    throw new RuntimeException("No such server by that port");
                server.stopServer();
                httpServers.remove(port);
            }
        }
        catch (Exception e)
        {
            throw new FactoidException(
                    "Exception occurred while stopping an http server on port " + port);
        }
    }
    
    public static final File serverPortsFile = new File("storage", "serverports.txt");
    public static final File maxServersFile = new File("storage", "maxservers.txt");
    
    /**
     * Throws an exception if a server cannot be started on the specified port. The
     * reasons for throwing an exception are, at present:<br/>
     * <ul>
     * <li>HTTP servers are disabled because storage/serverports.txt does not exist</li>
     * <li>The specified port does not match the regex in storage/serverports.txt</li>
     * <li>There are too many servers already started</li>
     * </ul>
     * 
     * @param port
     */
    private static void verifyStartServer(int port)
    {
        if (!serverPortsFile.exists())
            throw new RuntimeException("HTTP servers are disabled. To enable them, create "
                + "a file called serverports.txt in the bot's storage folder, and "
                + "set its contents to be a regular expression that will match the "
                + "port numbers you want to allow servers to be started on.");
        String regex = StringUtils.readFile(serverPortsFile);
        regex = regex.trim();
        if (!("" + port).matches(regex))
            throw new RuntimeException("Invalid port; the port has to match the regex "
                + regex);
        int maxServers;
        if (maxServersFile.exists())
            maxServers = Integer.parseInt(StringUtils.readFile(maxServersFile).trim());
        else
            maxServers = 20;
        if (httpServers.size() >= maxServers)
            throw new RuntimeException("There are already " + httpServers.size()
                + " servers started. This bot imposes a " + "maximum limit of "
                + maxServers + " at a time.");
    }
    
    static
    {
        HelpSystem.installProvider(new DefaultHelpProvider());
        HelpSystem.installProvider(new FunctionHelpProvider());
        HelpSystem.installProvider(new PropsHelpProvider("docs/help.props"));
    }
    
    public static Map<String, Evaluator> evalEngines = new HashMap<String, Evaluator>();
    
    public static void registerEvalEngine(Evaluator engine)
    {
        evalEngines.put(engine.getName(), engine);
    }
    
    static
    {
        registerEvalEngine(new JepliteEvaluator());
        registerEvalEngine(new JEvalEvaluator());
        registerEvalEngine(new CaltechEvaluator());
    }
    
    public static Evaluator getEvalEngine(String name)
    {
        Evaluator engine = evalEngines.get(name);
        if (engine == null)
            throw new RuntimeException("Invalid evaluator engine name: " + name
                + ", expected one of "
                + StringUtils.delimited(evalEngines.keySet().toArray(new String[0]), ", "));
        return engine;
    }
    
    public static Evaluator getDefaultEvalEngine(String channel)
    {
        /*
         * I'm going to write a custom eval engine using a mix of Python and Java
         * (assuming I can get PyParsing to work in Jython), with the Python side parsing
         * the equation and the Java side actually resolving it (the Java side will cache
         * the parsed representation, so the Python side will only be invoked once in a
         * while, which should save on performance). Then I'll get rid of the option of
         * having an eval engine. So for now we're just hard-coding it to jeval, which is
         * the default anyway.
         */
        return getEvalEngine("jeval");
    }
    
    public static class FutureFactoid implements Runnable
    {
        private int delay;
        private String server;
        private String channel;
        private ArgumentList arguments;
        private UserMessenger sender;
        private Messenger source;
        private String key;
        private FactQuota quota;
        public long startTime;
        
        public FutureFactoid(int delay, String server, String channel,
                ArgumentList arguments, UserMessenger sender, Messenger source, String key,
                FactQuota quota)
        {
            if (delay > (86400 * 2))
                throw new RuntimeException("Futures can't be scheduled more than 2 days ("
                    + (86400 * 2) + " seconds) into the future. You're "
                    + "trying to schedule " + "a future to run sooner than that.");
            // // FIXME: This needs to be changed so that future factoids can be scheduled
            // in
            // // factoids that are not channel-scoped.
            // if (channel == null)
            // throw new RuntimeException("Can't schedule future factoids in pm. "
            // + "Run this factoid at a channel.");
            this.delay = delay;
            this.server = server;
            this.channel = channel;
            this.arguments = arguments;
            this.sender = sender;
            this.source = source;
            this.key = key;
            this.quota = quota;
            startTime = System.currentTimeMillis() + (delay * 1000);
            /*
             * We want to force the arguments to be evaluated now, instead of when the
             * future is run
             */
            for (int i = 0; i < arguments.length(); i++)
                arguments.get(i, new NullSink());
        }
        
        public void run()
        {
            // try
            // {
            // startTime = System.currentTimeMillis() + (delay * 1000);
            // Thread.sleep(delay * 1000);
            // }
            // catch (InterruptedException e)
            // {
            // }
            synchronized (futureFactoidLock)
            {
                if (futureFactoids.get(key) != this)
                    return;
                futureFactoids.remove(key);
                TimedKillThread tkt = new TimedKillThread(Thread.currentThread());
                tkt.start();
                String result;
                try
                {
                    result =
                            doFactImport(server, channel, arguments, sender, source, true,
                                    quota, ImportLevel.any);
                }
                catch (Throwable t)
                {
                    result = "A future error occured: " + PastebinUtils.pastebinStack(t);
                }
                finally
                {
                    tkt.active = false;
                }
                if (result.trim().equals(""))
                    return;
                // TODO: this doesn't properly catch the case where the server is
                // nonexistent or the server is not connected. Add code to deal with these
                // two cases.
                sendActionOrMessage(source, result);
            }
        }
        
        public void start()
        {
            futureFactoidPool.schedule(this, delay, TimeUnit.SECONDS);
        }
    }
    
    public static HashMap<String, FutureFactoid> futureFactoids =
            new HashMap<String, FutureFactoid>();
    public static final Object futureFactoidLock = new Object();
    public static ScheduledThreadPoolExecutor futureFactoidPool =
            new ScheduledThreadPoolExecutor(1);
    public static final HashMap<String, List<Command>> commands =
            new HashMap<String, List<Command>>();
    // numeric 320: is signed on as account
    public static ProxyStorage<Storage> proxyStorage;
    public static Storage storage;
    
    public static Config config;
    public static boolean isRunning = true;
    
    public static volatile boolean logQueueRunning = true;
    
    public static void main(String[] args) throws Throwable
    {
        System.out.println("This is JZBot, http://jzbot.googlecode.com");
        // System.out.println("Revision " + VersionInfo.revision + ", built on "
        // + VersionInfo.shortDateString);
        System.out.println("Written by a whole bunch of people, see");
        System.out.println("http://code.google.com/p/jzbot/wiki/People for "
            + "the full list.");
        System.out.println();
        System.out.println("--- Stats ---");
        System.out.println("JVM path: " + System.getProperty("java.home"));
        System.out.println("AUTOBUS_SERVER env: " + System.getenv("AUTOBUS_SERVER"));
        System.out.println("autobus.server sysprop: "
            + System.getProperty("autobus.server"));
        System.out.println();
        if (args.length > 0)
        {
            doWithArguments(args);
            return;
        }
        start();
    }
    
    private static void doWithArguments(String[] args)
    {
        if (args[0].equals("help"))
        {
            System.out.println("JZBot is an IRC bot. If you have questions, connect");
            System.out.println("to irc.freenode.net and join channel #jzbot.");
            System.out
                    .println("To set up your bot, run \"jzbot addserver <name> <protocol> <server> ");
            System.out.println("<port> <nick> <hostname> <password>\". <server>");
            System.out.println("is the IRC server to connect to. For example, this");
            System.out.println("could be \"irc.freenode.net\". <port> is the port");
            System.out.println("on the server to connect to. This is usually 6667.");
            System.out.println("<nick> is the nickname to use on that server. ");
            System.out.println("<hostname> is your hostname or hostmask on the IRC");
            System.out.println("server, which the bot will use to allow you to ");
            System.out.println("tell it to join channels, leave channels, create ");
            System.out.println("factoids, and so on. <password> is the password");
            System.out.println("you want the bot to use when connecting to the");
            System.out.println("server. <password> is entirely optional. <name> ");
            System.out.println("is a name for your server. You'll use this to refer");
            System.out.println("to your server when communicating with your bot. This");
            System.out.println("can be anything.");
            System.out.println("");
            System.out.println("Once you've set up the bot successfully, run \"jzbot\"");
            System.out.println("to actually start your bot.");
            System.out.println("");
            System.out.println("Advanced users can also do \"jzbot addsuperop <server> "
                + "<hostname>\"");
            System.out.println("or \"jzbot config\" or \"jzbot config <varname>\" or");
            System.out.println("\"jzbot config <varname> <newvalue>\" or \"jzbot ");
            System.out.println("activateserver <servername>\".");
        }
        else if (args[0].equals("addserver"))
        {
            ArrayList<String> list = new ArrayList<String>(Arrays.asList(args));
            list.remove(0);
            args = list.toArray(new String[0]);
            if (args.length < 6 || args.length > 7)
            {
                System.out.println("\"jzbot addserver\" expects either 6 or 7 "
                    + "arguments, but you provided " + args.length);
                System.out.println("arguments. See \"jzbot help\" for help.");
                return;
            }
            String serverName = args[0];
            String protocol = args[1];
            String server = args[2];
            String portString = args[3];
            String nick = args[4];
            String hostname = args[5];
            String password = (args.length > 6 ? args[6] : "");
            if (!isOkServerName(serverName))
            {
                System.out.println("That server name is not valid. Server names must");
                System.out.println("contain only lowercase letters, numbers, and hyphens.");
                return;
            }
            int port;
            try
            {
                port = Integer.parseInt(portString);
            }
            catch (NumberFormatException e)
            {
                System.out.println("You specified " + portString + " for the port, but");
                System.out.println("the port must be a number.");
                return;
            }
            if (port < 0 || port > 65535)
            {
                System.out.println("The port number you specified (" + port + "), should");
                System.out.println("have been within the range 0 - 65535, but it was not.");
                return;
            }
            System.out.println("Ok, it looks like the information you provided ");
            System.out.println("will work. Hang on a sec while I set everything up.");
            System.out.println("");
            initProxyStorage();
            if (storage.getServer(serverName) != null)
            {
                System.out.println("There is already a server using that name. ");
                System.out.println("Try another name.");
                return;
            }
            Server datastoreServer = storage.createServer();
            datastoreServer.setActive(true);
            datastoreServer.setName(serverName);
            datastoreServer.setProtocol(protocol);
            datastoreServer.setNick(nick);
            datastoreServer.setPassword(password);
            datastoreServer.setPort(port);
            datastoreServer.setServer(server);
            Operator op = storage.createOperator();
            op.setHostname(hostname);
            datastoreServer.getOperators().add(op);
            storage.getServers().add(datastoreServer);
            Notify.serverAdded.fireListeners(ScopeLevel.server, "@" + serverName, false);
            System.out.println("");
            System.out.println("A new server has been added. Run \"jzbot\"");
            System.out.println("to start your bot.");
        }
        else if (args[0].equals("config"))
        {
            initProxyStorage();
            // TODO: uncomment this and make it so it mirrors the ~config command in usage
            // (and perhaps it could even invoke ~config directly)
            // if (args.length == 1)
            // {
            // System.out.println("All config var names: "
            // + StringUtils.delimited(ConfigVars.values(), new ToString<ConfigVars>()
            // {
            //
            // @Override
            // public String toString(ConfigVars object)
            // {
            // return object.name();
            // }
            // }, ", "));
            // }
            // else if (args.length == 2)
            // {
            // System.out.println("Config variable \"" + args[1]
            // + "\" is currently set to \"" + ConfigVars.valueOf(args[1]).get());
            // }
            // else
            // {
            // ConfigVars.valueOf(args[1]).set(args[2]);
            // System.out.println("Successfully set the var \"" + args[1]
            // + "\" to the value \"" + args[2] + "\".");
            // }
        }
        else if (args[0].equals("activateserver"))
        {
            if (args.length == 1)
            {
                System.out.println("You must specify the name of a server to activate.");
                return;
            }
            initProxyStorage();
            Server server = storage.getServer(args[1]);
            if (server == null)
            {
                System.out.println("There is no such server with that name. The "
                    + "servers that are currently in your database are:");
                for (Server s : storage.getServers().isolate())
                {
                    System.out.println("    " + s.getName());
                }
                return;
            }
            server.setActive(true);
            System.out.println("The server has been successfully "
                + "set to active. The next time JZBot starts, "
                + "it will connect to that server.");
        }
        else if (args[0].equals("addsuperop"))
        {
            if (args.length < 3)
            {
                System.out.println("You need to specify a server and the hostname");
                System.out.println("of the superop to add.");
            }
            String serverName = args[1];
            String hostname = args[2];
            initProxyStorage();
            Server server = storage.getServer(serverName);
            if (server == null)
            {
                System.out.println("There is no such server.");
                return;
            }
            if (server.getOperator(hostname) != null)
            {
                System.out.println("That hostname is already a superop on that server.");
            }
            Operator op = storage.createOperator();
            op.setHostname(hostname);
            server.getOperators().add(op);
            System.out.println("That user was successfully added as a superop.");
        }
        // else if (args[0].equals("switchnick"))
        // {
        // if (args.length > 1)
        // {
        // initProxyStorage();
        // config.setNick(args[1]);
        // System.out.println("Successfully set the bot's nick to be \"" + args[1]
        // + "\".");
        // }
        // else
        // {
        // System.out.println("You need to specify a new nickname for the "
        // + "bot to use. Try \"jzbot switchnick <newnick>\".");
        // }
        // }
        else
        {
            System.out.println("That's an invalid command. Try \"jzbot help\".");
        }
    }
    
    private static boolean isOkServerName(String serverName)
    {
        return serverName.matches("^[a-z0-9\\-]*$");
    }
    
    private static void loadCommands()
    {
        installCommand(new CommandListCommand());
        installCommand(new ConfigCommand());
        installCommand(new CrosstalkCommand());
        installCommand(new ExecCommand());
        installCommand(new FactoidCommand());
        // loadCommand(new GoogleCommand());
        installCommand(new HelpCommand());
        installCommand(new JoinCommand());
        installCommand(new LeaveCommand());
        // loadCommand(new LengthCommand());
        installCommand(new ListChannelsCommand());
        installCommand(new MMCommand());
        installCommand(new PluginCommand());
        installCommand(new RedefineCommand());
        installCommand(new RegexCommand());
        installCommand(new RestartCommand());
        // loadCommand(new RouletteCommand());
        // loadCommand(new SayCommand());
        installCommand(new ScopeCommand());
        installCommand(new ServerCommand());
        installCommand(new ShutdownCommand());
        installCommand(new StatusCommand());
        installCommand(new SuperopCommand());
        installCommand(new SwitchnickCommand());
        installCommand(new TriggerCommand());
        // loadCommand(new TTTCommand());
        installCommand(new UpdateCommand());
        // loadCommand(new WeatherCommand());
    }
    
    /**
     * Loads the specified command. If there is already a command with the specified name,
     * chain loading is used; the already-existing command will be invoked first, but if
     * it says that the input data is not relevant (as per
     * {@link Command#relevant(String, String, boolean, UserMessenger, Messenger, String)}
     * , then this command will be called. If there are multiple commands with the same
     * name, the first one added will be tried first, then the next, then the next, and so
     * on.
     * 
     * @param command
     *            The command to load
     */
    public static void installCommand(Command command)
    {
        String name = command.getName();
        synchronized (commands)
        {
            List<Command> list;
            if (commands.containsKey(name))
                list = commands.get(name);
            else
            {
                list = new ArrayList<Command>(1);
                commands.put(name, list);
            }
            list.add(command);
        }
    }
    
    private static void start() throws Throwable
    {
        System.out.println("Initializing...");
        addShutdownHook();
        DefaultPastebinProviders.installDefaultSet();
        logsFolder.mkdirs();
        Options.includeJavaStackInExceptions = true;
        Options.showJavaExceptions = true;
        System.out.println("Starting the ProxyStorage system...");
        initProxyStorage();
        if (storage.getServers().size() == 0)
        {
            System.out.println();
            System.out.println("No servers have been added. JZBot will exit.");
            System.out.println("Run \"jzbot help\" for help on how to add a");
            System.out.println("server for your bot to connect to. Specifically,");
            System.out.println("you'll want to look at the addserver command.");
            System.exit(0);
        }
        System.out.println("Loading configuration...");
        loadConfiguration();
        System.out.println("Starting the relational data store...");
        startRelationalStore();
        System.out.println("Loading built-in protocols...");
        loadProtocols();
        System.out.println("Loading built-in commands...");
        loadCommands();
        System.out.println("Starting log sink...");
        startLogSinkThread();
        System.out.println("Loading cached regular expressions...");
        reloadRegexes();
        System.out.println("Starting the plugin system...");
        PluginSystem.start();
        System.out.println("Starting the automatic restart thread...");
        startAutomaticRestartThread();
        System.out.println("Starting the pm user time thread...");
        startPmUserTimeThread();
        System.out.println("Running _onstartup notifications...");
        runNotificationFactoid(null, null, null, null, "", "", "", "_onstartup",
                new String[0], true, false);
        System.out.println("Firing initial notification events...");
        fireInitialNotifyEvents();
        System.out.println("Starting connection cycle thread...");
        startConnectionCycleThread();
        System.out.println("Dispatching notifications to connection cycle thread...");
        notifyConnectionCycleThread();
        System.out.println();
        System.out.println("JZBot has successfully started up. Server "
            + "connections will be established in a few seconds.");
        System.out.println();
    }
    
    private static void fireInitialNotifyEvents()
    {
        for (Server server : storage.getServers().isolate())
        {
            Notify.serverAdded.fireListeners(ScopeLevel.server, "@" + server.getName(),
                    true);
            for (Channel channel : server.getChannels().isolate())
            {
                Notify.channelAdded.fireListeners(ScopeLevel.channel,
                        "@" + server.getName() + channel.getName(), true);
            }
        }
    }
    
    private static void loadProtocols()
    {
        /*
         * The five built-in protocols
         */
        ProtocolManager.installProtocol(new IrcProtocol());
        ProtocolManager.installProtocol(new XmppProtocol());
        ProtocolManager.installProtocol(new BZFlagProtocol());
        ProtocolManager.installProtocol(new FacebookProtocol());
        ProtocolManager.installProtocol(new ImapProtocol());
    }
    
    private static void startPmUserTimeThread()
    {
        pmUserScopeTimeoutThread.setDaemon(true);
        pmUserScopeTimeoutThread.start();
    }
    
    public static boolean shouldRestartOnShutdown = false;
    
    private static void addShutdownHook()
    {
        Runtime.getRuntime().addShutdownHook(new Thread()
        {
            public void run()
            {
                System.out.println();
                System.out.println("Shutting down the plugin manager...");
                PluginSystem.shutdown();
                System.out.println("Shutting down the log queue...");
                synchronized (logQueueLock)
                {
                    logQueueRunning = false;
                    int discarded = (logQueue == null ? 0 : logQueue.size());
                    System.out.println("Log queue has shut down. " + discarded
                        + " log event" + (discarded == 1 ? " was" : "s were")
                        + " discarded.");
                }
            }
        });
    }
    
    public static java.sql.Connection relationalStore;
    
    public static void startRelationalStore()
    {
        try
        {
            Class.forName("org.h2.Driver");
            File location = new File("storage/relational/rs");
            relationalStore =
                    DriverManager.getConnection("jdbc:h2:" + location.getPath()
                        + ";FILE_LOCK=SOCKET", "sa", "");
            Statement statement = relationalStore.createStatement();
            // statement.execute("create user if not exists jzbot password 'pass'");
            // Now we'll set up aliases to all public static methods in
            // PublicDatabaseUtils.
            Method[] methods = PublicDatabaseUtils.class.getMethods();
            for (Method method : methods)
            {
                if (method.getName().startsWith("metadata_"))
                {
                    statement.execute("create alias if not exists " + method.getName()
                        + " for \"jw.jzbot.PublicDatabaseUtils." + method.getName() + "\"");
                }
            }
            statement.close();
            // relationalStore.close();
            // relationalStore =
            // DriverManager.getConnection("jdbc:h2:" + location.getPath()
            // + ";FILE_LOCK=SOCKET", "jzbot", "pass");
        }
        catch (Exception e)
        {
            try
            {
                relationalStore.close();
            }
            catch (Exception ex)
            {
                ex.printStackTrace();
            }
            relationalStore = null;
            throw new RuntimeException("Could not connect to the relational data store.", e);
        }
    }
    
    private static void initProxyStorage()
    {
        proxyStorage =
                new ProxyStorage<Storage>(Storage.class, new File("storage/db"), 600, 1000,
                        700, 300, 400);
        storage = proxyStorage.getRoot();
        config = storage.getConfig();
        if (config == null)
        {
            config = storage.createConfig();
            storage.setConfig(config);
        }
    }
    
    private static void loadConfiguration() throws Exception
    {
        Configuration.initialize();
        registerDefaultConfigVars();
        logQueue = new LinkedBlockingQueue<LogEvent>(Configuration.getInt("", "lqmaxsize"));
        Configuration.addListener("", "proxytrace", new VarListener()
        {
            
            @Override
            public void changed(String scope, String name)
            {
                proxyTraceConfigChanged();
            }
        });
        proxyTraceConfigChanged();
    }
    
    private static void registerDefaultConfigVars() throws IOException
    {
        new PythonInterpreter().execfile(new FileInputStream(
                "src/jw/jzbot/configuration/default_config_vars.py"));
    }
    
    public static void onJoin(Server datastoreServer, String serverName, String channel,
            String sender, String login, String hostname)
    {
        System.out.println("join detected at " + serverName + " on " + channel + " by "
            + sender);
        Channel chan = datastoreServer.getChannel(channel);
        if (chan == null)
            return;
        logEvent(serverName, channel, "joined", sender, login + "@" + hostname);
        if (sender.equals(getRealConnection(serverName).getConnection().getNick()))
        {
            runNotificationFactoid(serverName, datastoreServer, channel, chan, sender,
                    login, hostname, "_selfjoin", null, true, true);
        }
        else
        {
            runNotificationFactoid(serverName, datastoreServer, channel, chan, sender,
                    login, hostname, "_onjoin", null, true, true);
        }
    }
    
    public static void onPart(Server datastoreServer, String serverName, String channel,
            String sender, String login, String hostname)
    {
        logEvent(serverName, channel, "left", sender, "Left the channel");
        runNotificationFactoid(serverName, datastoreServer, channel, null, sender, login,
                hostname, "_onpart", null, true, true);
    }
    
    public static void onBeforeQuit(Server datastoreServer, String serverName,
            String sourceNick, String sourceLogin, String sourceHostname, String reason)
    {
        Connection con = getRealConnection(serverName).getConnection();
        for (String channel : con.getChannels())
        {
            if (getUser(con, channel, sourceNick) != null)
            {
                logEvent(serverName, channel, "left", sourceNick, "Quit: " + reason);
                runNotificationFactoid(serverName, datastoreServer, channel, null,
                        sourceNick, sourceLogin, sourceHostname, "_onquit", null, true,
                        true);// should cascade here be
                // false? (to rely on the quit notification itself)
            }
        }
    }
    
    public static HashMap<String, String> channelTopics = new HashMap<String, String>();
    
    public static void onTopic(Server datastoreServer, String serverName, String channel,
            String topic, String setBy, String setByUsername, String setByHostname,
            long date, boolean changed)
    {
        channelTopics.put("@" + serverName + channel, topic);
        if (changed)
        {
            logEvent(serverName, channel, "topic", setBy, topic);
            runNotificationFactoid(serverName, datastoreServer, channel, null, setBy,
                    setByUsername, setByHostname, "_ontopic", new String[] { topic,
                            "" + date }, true, true);
        }
    }
    
    public static void onMode(Server datastoreServer, String serverName, String channel,
            String sourceNick, String sourceLogin, String sourceHostname, String mode)
    {
        logEvent(serverName, channel, "mode", sourceNick, mode);
        runNotificationFactoid(serverName, datastoreServer, channel, null, sourceNick,
                sourceLogin, sourceHostname, "_onmode", new String[] { mode }, true, true);
    }
    
    public static void onNickChange(Server datastoreServer, String serverName,
            String oldNick, String login, String hostname, String newNick)
    {
        Connection con = getRealConnection(serverName).getConnection();
        for (String channel : con.getChannels())
        {
            if (getUser(con, channel, newNick) != null)
            {
                logEvent(serverName, channel, "nick", oldNick, newNick);
                runNotificationFactoid(serverName, datastoreServer, channel, null, newNick,
                        login, hostname, "_onrename", new String[] { oldNick, newNick },
                        true, true);
            }
        }
    }
    
    /**
     * Runs the specified notification factoid.
     * 
     * @param channelName
     *            The channel the event occurred at. This must either be the name of a
     *            channel that the bot is currently at, or null. If it's null, then this
     *            means this "event" is a global event, and the notification factoids will
     *            be run at all channels (and if there are any global notification
     *            factoids for this event, they will be run too).
     * @param chan
     *            The channel object. Normally this is null. Since obtaining a Channel
     *            object from the database is somewhat expensive, then if the code that
     *            calls this method already has a Channel object it should generally pass
     *            it in here. This channel's name must be the same as <tt>channelName</tt>
     *            , and this must be null if <tt>channelName</tt> is null.
     * @param sender
     *            The person that triggered the event. When the notification factoids are
     *            run, %0% and %who% will be set to the value of this parameter.
     * @param factname
     *            The prefix of the notification factoid to run. All factoids whose names
     *            are either equal to this name or whose names start with this name plus
     *            an underscore will be run. For example, if <tt>factname</tt> is "_test",
     *            then the factoids "_test", "_test_something", and "_test_other" would be
     *            run, but not the factoid "_testsomething".
     * @param args
     *            The arguments to pass to the factoids when they are run. The first
     *            argument will be put into the local varaible %1%, the second argument
     *            will be put into %2%, and so on. This can be either null or an array
     *            with no items in it to indicate that there should be no arguments.
     * @param timed
     *            True if this is timed, false if it is not. This should almost always be
     *            true. If this is true, then a TimedKillThread will be started that will
     *            forcibly terminate the factoids if they run for more than 60 seconds. If
     *            this is false, the factoids will never be forcibly terminated, which
     *            could freeze the bot if the factoids have an infinte loop or some such
     *            other problem.
     */
    private static void runNotificationFactoid(String serverName, Server server,
            String channelName, Channel chan, String sender, String username,
            String hostname, String factname, String[] args, boolean timed, boolean cascade)
    {
        System.out.println("Running notification factoid on server " + serverName
            + " channel " + channelName + ", with sender " + sender + ": " + factname);
        if (!factname.startsWith("_"))
            System.err.println("Factoid notification name \"" + factname
                + "\" doesn't start with an underscore. All factoid "
                + "notification names must start with an underscore.");
        if (args == null)
            args = new String[0];
        ArrayList<Factoid> facts = new ArrayList<Factoid>();
        if (serverName == null)
        {
            facts.addAll(Arrays.asList(storage.searchFactoids(factname)));
            facts.addAll(Arrays.asList(storage.searchFactoids(factname + "_*")));
        }
        else if (channelName == null)
        {
            if (server == null)
                server = storage.getServer(serverName);
            if (server == null)
                return;
            facts.addAll(Arrays.asList(server.searchFactoids(factname)));
            facts.addAll(Arrays.asList(server.searchFactoids(factname + "_*")));
            if (cascade)
            {
                facts.addAll(Arrays.asList(storage.searchFactoids("_server" + factname)));
                facts.addAll(Arrays.asList(storage.searchFactoids("_server" + factname
                    + "_*")));
            }
        }
        else
        {
            if (server == null)
                server = storage.getServer(serverName);
            if (server == null)
                return;
            if (chan == null)
                chan = server.getChannel(channelName);
            if (chan == null)
                return;
            facts.addAll(Arrays.asList(chan.searchFactoids(factname)));
            facts.addAll(Arrays.asList(chan.searchFactoids(factname + "_*")));
            if (cascade)
            {
                facts.addAll(Arrays.asList(server.searchFactoids("_chan" + factname)));
                facts.addAll(Arrays.asList(server.searchFactoids("_chan" + factname + "_*")));
                facts.addAll(Arrays.asList(storage.searchFactoids("_chan" + factname)));
                facts.addAll(Arrays.asList(storage
                        .searchFactoids("_chan" + factname + "_*")));
            }
        }
        TimedKillThread tkt = new TimedKillThread(Thread.currentThread());
        // FIXME: make this configurable (up to a hard-coded limit of, say, 3 minutes) by
        // a config variable, and then default it to 40 seconds or something
        tkt.maxRunTime = 75 * 1000;
        if (timed)
            tkt.start();
        try
        {
            for (Factoid f : facts)
            {
                if (f != null)
                {
                    incrementIndirectRequests(f);
                    // FIXME: this means notification factoids can't tell if a user is a
                    // superop. We need to somehow include the hostname of the user here.
                    // UPDATE: how the heck can a user running a notification factoid be a
                    // superop? Notification factoids are run spontaneously by the server,
                    // without any user input, so there isn't a "user" that runs them. On
                    // the other hand, some notifications are triggered by users (like
                    // when a user joins); maybe this is what I was thinking about.
                    String pseudoChannel = channelName;
                    String pseudoServer = serverName;
                    if (pseudoServer == null || pseudoChannel == null)
                    {
                        String primary = Configuration.getText(null, "primary");
                        try
                        {
                            pseudoServer = extractServerName(primary);
                            pseudoChannel = extractChannelName(primary);
                        }
                        catch (Exception e)
                        {
                            new Exception(
                                    "Exception occurred while extracting "
                                        + "primary channel in notification factoid. The notification "
                                        + "will run, but output generated by it will be silently discarded.",
                                    e).printStackTrace();
                        }
                    }
                    String factValue =
                            safeRunFactoid(f, server, serverName, channelName,
                                    new UserMessenger(serverName, sender, username,
                                            hostname), new ChannelScope(pseudoServer,
                                            pseudoChannel), args, true,
                                    new HashMap<String, String>());
                    ConnectionWrapper con = getConnection(pseudoServer);
                    if (con != null)
                    {
                        if (factValue.trim().equals(""))
                            ;
                        else if (factValue.startsWith("<ACTION>"))
                            con.sendAction(pseudoChannel,
                                    factValue.substring("<ACTION>".length()));
                        else
                            con.sendMessage(pseudoChannel, factValue);
                    }
                }
            }
        }
        finally
        {
            tkt.active = false;
        }
    }
    
    public static void sendActionOrMessage(ConnectionWrapper target, String channel,
            String message)
    {
        if (message.equals(""))
            ;
        else if (message.startsWith("<ACTION>"))
            target.sendAction(channel, message.substring("<ACTION>".length()));
        else
            target.sendMessage(channel, message);
    }
    
    public static void sendActionOrMessage(Messenger target, String message)
    {
        if (message.equals(""))
            ;
        else if (message.startsWith("<ACTION>"))
            target.sendAction(message.substring("<ACTION>".length()));
        else
            target.sendMessage(message);
    }
    
    public static void incrementIndirectRequests(Factoid f)
    {
        f.setIndirectRequests(f.getIndirectRequests() + 1);
    }
    
    public static void incrementDirectRequests(Factoid f)
    {
        // System.out.println("Incrementing direct requests for " +
        // f.getName());
        f.setDirectRequests(f.getDirectRequests() + 1);
        // System.out.println("incremented");
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
    public static String runFactoid(Factoid factoid, String server, String channel,
            UserMessenger sender, Messenger source, String[] args,
            Map<String, String> vars, boolean allowRestricted, FactQuota quota)
    {
        if (quota == null)
            quota = new FactQuota();
        if (allowRestricted == false && factoid.isRestricted())
            throw new FactoidException("The factoid " + factoid.getName()
                + " is restricted. Only ops and superops "
                + "can run it, as well as the bot itself.");
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
        vars.put("0", sender.nick());
        vars.put("who", sender.nick());
        if (channel != null)
            vars.put("channel", channel);
        String selfName = null;
        if (server != null)
        {
            vars.put("server", server);
            Connection realCon = getRealConnection(server).getConnection();
            selfName = realCon.getNick();
            vars.put("self", selfName);
        }
        vars.put("source", channel == null ? sender.nick() : channel);
        String text = factoid.getValue();
        String factoidName = factoid.getName();
        long startMillis = System.currentTimeMillis();
        FactEntity parsedFactoid = FactParser.parse(text, factoid.getName());
        long parsedMillis = System.currentTimeMillis();
        FactContext context = new FactContext();
        context.setQuota(quota);
        context.setChannel(channel);
        context.setSender(sender);
        context.setGlobalVars(globalVariables);
        context.setLocalVars(vars);
        context.setServer(server);
        context.setSelf(selfName);
        context.setSource(source);
        // Now we actually run the factoid.
        StringSink resultSink = new StringSink();
        parsedFactoid.resolve(resultSink, context);
        String result = resultSink.toString();
        long finishedMillis = System.currentTimeMillis();
        System.out.println(factoidName + ": Parsed in " + (parsedMillis - startMillis)
            + " ms, ran in " + (finishedMillis - parsedMillis) + " ms");
        // The factoid has been run. Now we return the value.
        boolean isAction = context.isAction();
        return (isAction ? "<ACTION>" : "") + result.toString();
    }
    
    public static enum ImportLevel
    {
        // FIXME: add a server level here
        any, exact, global
    }
    
    public static String doFactImport(String server, String channel,
            ArgumentList arguments, UserMessenger sender, Messenger source,
            boolean allowRestricted, FactQuota quota, ImportLevel level)
    {
        return doFactImport(server, channel, arguments, sender, source, allowRestricted,
                quota, level, null);
    }
    
    public static String doFactImport(String server, String channel,
            ArgumentList arguments, UserMessenger sender, Messenger source,
            boolean allowRestricted, FactQuota quota, ImportLevel level,
            Map<String, String> cascadingVars)
    {
        Factoid f = null;
        boolean channelSpecific = false;
        boolean serverSpecific = false;
        Server sv = null;
        Channel cn = null;
        String factname = arguments.getString(0);
        if (server != null)
        {
            sv = storage.getServer(server);
            if (channel != null)
                cn = sv.getChannel(channel);
        }
        // First, we'll try channel-specific lookup.
        if (cn != null)
        {
            f = cn.getFactoid(factname);
            if (f != null)
                channelSpecific = true;
        }
        // Now we'll try server-specific lookup.
        if (f == null && sv != null)
        {
            f = sv.getFactoid(factname);
            if (f != null)
                serverSpecific = true;
        }
        // Now we'll try global lookup.
        if (f == null)
        {
            f = JZBot.storage.getFactoid(factname);
        }
        // Make sure we got one
        if (f == null)
            throw new RuntimeException("That factoid (\"" + arguments.getString(0)
                + "\") doesn't exist at the server \"" + server + "\" and channel \""
                + channel + "\", so you can't import it.");
        Map<String, String> varMap = new HashMap<String, String>();
        if (cascadingVars != null)
            varMap.putAll(cascadingVars);
        incrementIndirectRequests(f);
        return runFactoid(f, server, channel, sender, source, arguments.subList(1)
                .evalToArray(), varMap, allowRestricted, quota);
    }
    
    public static void onKick(Server datastoreServer, String serverName, String channel,
            String kickerNick, String kickerLogin, String kickerHostname,
            String recipientNick, String reason)
    {
        logEvent(serverName, channel, "kick", kickerNick, recipientNick + " " + reason);
        runNotificationFactoid(serverName, datastoreServer, channel, null, kickerNick,
                kickerLogin, kickerHostname, "_onkick", new String[] { recipientNick,
                        reason, kickerNick }, true, true);
        if (recipientNick.equals(getRealConnection(serverName).getConnection().getNick()))
        {
            // FIXME: wait a configurable amount of time before rejoining, and maybe put
            // this into a thread pool executor to wait, and maybe try to rejoin every few
            // seconds if we can't on the first try. Also, this is really bad to operate
            // on the low-level connection since this join doesn't get logged; this needs
            // to be fixed as soon as possible.
            getRealConnection(serverName).getConnection().joinChannel(channel);
        }
    }
    
    public static void onMessage(Server datastoreServer, String serverName, String channel,
            String sender, String login, String hostname, String message)
    {
        logEvent(serverName, channel, "message", sender, message);
        TimedKillThread tkt = new TimedKillThread(Thread.currentThread());
        tkt.start();
        try
        {
            System.out.println("Message at " + serverName + " channel " + channel
                + " from " + sender + ": " + message);
            Channel chan = datastoreServer.getChannel(channel);
            if (chan == null)
            {
                System.out.println("No matching channel, probably means "
                    + "we've joined a channel without storage");
                return;
            }
            boolean processFactoids =
                    processChannelRegex(datastoreServer, serverName, channel, sender,
                            login, hostname, message, false);
            ConnectionContext context = getRealConnection(serverName);
            String trigger = chan.getTrigger();
            if (context != null)
                message = replaceNamePrefixWithTrigger(context, trigger, message);
            System.out.println("Message translated conformant to channel trigger: "
                + message);
            if (trigger != null && message.startsWith(trigger))
            {
                System.out.println("Trigger match! Message selected for processing.");
                try
                {
                    message = message.substring(trigger.length());
                    System.out.println("Message without trigger: " + message);
                    boolean replyToPseudo = false;
                    ChannelScope pseudoTarget = new ChannelScope(serverName, channel);
                    if (isSuperop(serverName, hostname))
                    {
                        if (message.startsWith("%"))
                        {
                            replyToPseudo = true;
                            message = message.substring("%".length());
                        }
                        if ((message.startsWith("@") || message.startsWith("#"))
                            && message.contains(" "))
                        {
                            pseudoTarget = parseFragment(message, pseudoTarget);
                            message = message.substring(message.indexOf(' ') + 1);
                        }
                        System.out.println("Message after pseudo-target processing: "
                            + message);
                    }
                    else
                        System.out.println("Sender is not a superop, so "
                            + "we're not performing any pseudo-target processing.");
                    // TODO: consider performing pseudo-target processing but throwing an
                    // exception if it yields anything and the user's not a superop
                    if (replyToPseudo && pseudoTarget.getChannelName() == null)
                    {
                        getConnection(serverName).sendMessage(
                                channel,
                                "You can only instruct the bot to reply to "
                                    + "the target if the target contains a channel.");
                        return;
                    }
                    System.out.println("Running message command");
                    runMessageCommand(
                            JZBot.storage.getServer(pseudoTarget.getServerName()),
                            pseudoTarget.getServerName(), pseudoTarget.getChannelName(),
                            false, datastoreServer, serverName, sender,
                            replyToPseudo ? pseudoTarget : new ChannelScope(serverName,
                                    channel), hostname, login, message, processFactoids);
                }
                catch (Throwable e)
                {
                    e.printStackTrace();
                    getConnection(serverName).sendMessage(
                            channel,
                            "Internal upper-propegation error: "
                                + PastebinUtils.pastebinStack(e) + " -- "
                                + e.getClass().getName() + ": " + e.getMessage());
                }
            }
            else
            {
                System.out.println("Incorrect trigger");
            }
        }
        catch (FactTimeExceededError e)
        {
            getConnection(serverName).sendMessage(channel,
                    "Time exceeded: " + PastebinUtils.pastebinStack(e));
        }
        finally
        {
            tkt.active = false;
        }
    }
    
    public static String NAME_SUFFIXES = ":,!";
    
    private static String replaceNamePrefixWithTrigger(ConnectionContext context,
            String trigger, String message)
    {
        String oldMessage = message;
        if (message.startsWith(context.getConnection().getNick()))
        {
            message = message.substring(context.getConnection().getNick().length());
            if (message.length() > 1 && NAME_SUFFIXES.contains("" + message.charAt(0)))
            {
                if (message.charAt(1) == ' ')
                    return trigger + message.substring(2);
            }
        }
        return oldMessage;
    }
    
    public static void onNotice(Server datastoreServer, String serverName,
            String sourceNick, String sourceLogin, String sourceHostname, String target,
            String line)
    {
        Connection con = getRealConnection(serverName).getConnection();
        boolean inPm = target.equals(con.getNick());
        if (!inPm && target.startsWith("#"))
            logEvent(serverName, target, "notice", sourceNick, line);
        runNotificationFactoid(serverName, datastoreServer, inPm ? null : target, null,
                sourceNick, sourceLogin, sourceHostname, "_onnotice", new String[] { line,
                        target }, true, true);
        System.out.println("Notice to " + target + " by " + sourceNick + ": " + line);
        
    }
    
    public static void onInvitation(Server datastoreServer, String serverName,
            String channel, String sender, String login, String hostname, String toChannel)
    {
        UserMessenger source = new UserMessenger(serverName, sender, login, hostname);
        try
        {
            Command command = commands.get("join").get(0);
            command.run(serverName, channel, true, source, source, toChannel
                + " frominvite");
        }
        catch (Throwable e)
        {
            if (e instanceof ResponseException)
                source.sendSpaced(e.getMessage());
            else
                source.sendSpaced("An error occurred while processing your invitation: "
                    + PastebinUtils.pastebinStack(e));
        }
    }
    
    public static boolean processChannelRegex(Server server, String serverName,
            String channel, String sender, String username, String hostname,
            String message, boolean action)
    {
        try
        {
            boolean factOverridden = false;
            synchronized (regexLock)
            {
                Map<String, List<String>> smap = regexCache.get(serverName);
                List<String> channelList = (smap == null ? null : smap.get(channel));
                if (channelList == null)
                    return true;
                for (String regex : channelList)
                {
                    Pattern pattern = Pattern.compile(regex);
                    Matcher matcher = pattern.matcher(message);
                    if (!matcher.find())
                        continue;
                    /*
                     * We found something.
                     */
                    OverrideStatus override =
                            runRegex(server, serverName, channel, sender, username,
                                    hostname, message, matcher, regex, action);
                    if (override == OverrideStatus.override)
                        return false;
                    else if (override == OverrideStatus.factoverride)
                        factOverridden = true;
                }
            }
            if (factOverridden)
                return false;
            return true;
        }
        catch (Throwable e)
        {
            e.printStackTrace();
            getConnection(serverName).sendMessage(channel,
                    "Pre-process regex error: " + PastebinUtils.pastebinStack(e));
            return true;
        }
    }
    
    public static enum OverrideStatus
    {
        override, factoverride, none
    }
    
    /**
     * 
     * @param server
     * @param channel
     * @param sender
     * @param hostname
     * @param message
     * @param action
     * @param regex
     * @return True if this overrides, false if it doesn't
     */
    private static OverrideStatus runRegex(Server server, String serverName,
            String channel, String sender, String username, String hostname,
            String message, Matcher matcher, String regexValue, boolean action)
    {
        Channel c = server.getChannel(channel);
        if (c == null)
            return OverrideStatus.none;
        Regex regex = c.getRegex(regexValue);
        if (regex == null)
            return OverrideStatus.none;
        Factoid f = c.getFactoid(regex.getFactoid());
        if (f == null)
            f = server.getFactoid(regex.getFactoid());
        if (f == null)
            f = JZBot.storage.getFactoid(regex.getFactoid());
        if (f == null)
        {
            getConnection(serverName).sendMessage(channel,
                    "Invalid factoid in regex " + regexValue + ": " + regex.getFactoid());
            return OverrideStatus.none;
        }
        HashMap<String, String> vars = new HashMap<String, String>();
        vars.put("regex", regexValue);
        vars.put("original", message);
        vars.put("matched", matcher.group(0));
        vars.put("hostname", hostname);
        vars.put("isaction", action ? "1" : "0");
        String[] strings = new String[matcher.groupCount()];
        for (int i = 1; i <= matcher.groupCount(); i++)
        {
            strings[i - 1] = matcher.group(i);
        }
        incrementIndirectRequests(f);
        String factValue =
                safeRunFactoid(f, server, serverName, channel, new UserMessenger(
                        serverName, sender, username, hostname), new ChannelScope(
                        serverName, channel), strings, true, vars);
        sendActionOrMessage(getConnection(serverName), channel, factValue);
        if ("true".equalsIgnoreCase(vars.get("__internal_override")))
            return OverrideStatus.override;
        else if ("true".equalsIgnoreCase(vars.get("__fact_override")))
            return OverrideStatus.factoverride;
        return OverrideStatus.none;
    }
    
    public static void onAction(Server datastoreServer, String serverName, String sender,
            String login, String hostname, String channel, String action)
    {
        System.out.println("action received on @" + serverName + "!" + sender + channel
            + ": " + action);
        if (!(channel.startsWith("#")))
            // We don't support actions sent in a pm right now. These might be supported
            // at a later time.
            return;
        logEvent(serverName, channel, "action", sender, action);
        TimedKillThread tkt = new TimedKillThread(Thread.currentThread());
        tkt.start();
        try
        {
            processChannelRegex(datastoreServer, serverName, channel, sender, login,
                    hostname, action, true);
        }
        catch (Throwable t)
        {
            t.printStackTrace();
            getConnection(serverName).sendMessage(sender,
                    "Exception while processing action: " + PastebinUtils.pastebinStack(t));
        }
        finally
        {
            tkt.active = false;
        }
    }
    
    /**
     * Figures out the commands that should be run for the specified message. The trigger
     * should be removed from the message before this method is called. This method will
     * take care of sending responses as needed for the command.
     * 
     * @param datastoreServer
     *            The server at which this command should be run. This is normally the
     *            same as <tt>senderServer</tt>, but if the server is explicitly specified
     *            in a message, then this will be the server specified in the message
     *            instead of the server the user is actually sending the command from.
     * @param serverName
     *            The name of the server represented by <tt>datastoreServer</tt>.
     * @param channel
     *            The name of the channel that this event should be run at, or null if
     *            this should be run just at a server.
     * @param pm
     *            True if the message was sent in a pm, false if it was sent to a channel
     * @param senderServer
     *            The server that the message was actually sent from
     * @param senderServerName
     *            The name of the server represented by <tt>senderServer</tt>
     * @param sender
     *            The name of the user that actually sent the message
     * @param source
     *            The source that the message came from. If the message was sent in a pm,
     *            then this will be representative of the person that sent the message. If
     *            the message was sent to a channel, then this will be representative of
     *            that channel.
     * @param hostname
     *            The hostname of the user that actually sent the message
     * @param username
     *            The username of the user that actually sent the message
     * @param message
     *            The message itself, with the trigger, if any, removed
     * @param processFactoids
     *            True if factoids should be processed, false if they should not. If this
     *            is false, then only commands (if applicable) will be run. Otherwise,
     *            commands and factoids (if applicable) will be run. Since this method
     *            does not handle regular expressions, this is generally used when a
     *            regular expression facility in an invoking method indicates that
     *            factoids should not be run (such as when a call to {factoverride}
     *            appears within a factoid).
     */
    private static void runMessageCommand(Server datastoreServer, String serverName,
            String channel, boolean pm, Server senderServer, String senderServerName,
            String sender, Messenger source, String hostname, String username,
            String message, boolean processFactoids)
    {
        threadLocalUsername.set(username);
        UserMessenger serverUser =
                new UserMessenger(senderServerName, sender, username, hostname);
        ChannelScope serverChannel = null;
        if (channel != null)
            serverChannel = new ChannelScope(serverName, channel);
        if (source == null)
        {
            if (pm)
                source = serverUser;
            else
                source = serverChannel;
        }
        // ConnectionWrapper con = getConnection(serverName);
        // ConnectionWrapper senderCon = getConnection(senderServerName);
        System.out.println("Starting command run for message " + message);
        String[] commandSplit = message.split(" ", 2);
        String command = commandSplit[0];
        String commandArguments = (commandSplit.length == 1 ? "" : commandSplit[1]);
        /*
         * We'll check to see if the text entered is a command. This is done before a
         * check to see if we're supposed to process factoids so that regexes can't
         * override commands.
         */
        List<Command> commandList = commands.get(command);
        if (commandList == null)
            commandList = Collections.EMPTY_LIST;
        for (Command c : commandList)
        {
            System.out.println("Checking command: " + c.getName());
            try
            {
                if (!c.relevant(serverName, channel, pm, serverUser, source,
                        commandArguments))
                {
                    System.out.println("Command is not relevant");
                    continue;
                }
                c.run(serverName, channel, pm, serverUser, source, commandArguments);
            }
            catch (Exception e)
            {
                if (e instanceof ResponseException)
                {
                    if (!(e instanceof DieException))
                    {
                        ResponseException re = (ResponseException) e;
                        if (!re.getMessage().equals(""))
                            source.sendSpaced(re.getMessage());
                    }
                }
                else
                {
                    e.printStackTrace();
                    source.sendSpaced("An error occured while running the command "
                        + command + ": " + PastebinUtils.pastebinStack(e) + " -- "
                        + e.getClass().getName() + ": " + e.getMessage());
                }
            }
            System.out.println("Finishing command run #1");
            return;
        }
        /*
         * If we get here, then the text isn't a command. We'll check to see if it's a
         * factoid.
         * 
         * Our first check will be for a channel-specific factoid. However, we should just
         * return if we're not supposed to process factoids, which occurrs if a regex
         * filtered out a call with override or factoverride.
         * 
         * And actually, that brings to mind another task that needs to be done, and I'm
         * putting it here in case I forget it: override and factoverride set local
         * varaibles prefixed with two underscores. Some mechanism that doesn't use local
         * variables needs to be added.
         */
        System.out.println("No relevant commands found");
        if (!processFactoids)
        {
            System.out.println("Finishing command run #2");
            return;
        }
        Factoid factoid = null;
        // Check for a channel-specific factoid
        if (channel != null)
        {
            Channel cn = datastoreServer.getChannel(channel);
            if (cn != null)
            {
                factoid = cn.getFactoid(command);
            }
        }
        // Check for a server-specific factoid
        if (factoid == null)
            factoid = datastoreServer.getFactoid(command);
        // Check for a global factoid
        if (factoid == null)
            factoid = storage.getFactoid(command);
        if (factoid != null)
        {
            System.out.println("Found a matching factoid: " + factoid.getName());
            if (factoid.isLibrary())
            {
                source.sendMessage("That factoid is a library factoid. It can only be run "
                    + "by importing it, by creating a regex "
                    + "that uses it, by using it as "
                    + "a trigger, and so on. Run \"factoid unlibrary " + command
                    + "\" if you want to remove this factoid's library status.");
                System.out.println("Finishing command run #5");
                return;
            }
            incrementDirectRequests(factoid);
            System.out.println("requests incremented");
            String factValue;
            System.out.println("calculating fact value");
            factValue =
                    safeRunFactoid(factoid, datastoreServer, serverName, channel,
                            serverUser, source, commandArguments.split(" "),
                            isSuperop(serverName, hostname), new HashMap<String, String>());
            System.out.println("fact value: " + factValue);
            sendActionOrMessage(source, factValue);
            System.out.println("Finishing command run #6");
            return;
        }
        System.out.println("invalid command, dispatching to invalid command processor");
        doInvalidCommand(pm, pm ? senderServer : datastoreServer, pm ? senderServerName
                : serverName, channel, serverUser, source);
        System.out.println("Finishing command run #7");
    }
    
    /**
     * Runs the specified factoid, returning its output. If an exception is thrown while
     * running the factoid, the exception's stack trace, along with some additional
     * information, is sent to pastebin.com, and an error message (including a url to the
     * pastebin post) returned instead of the factoid's output.
     * 
     * @param f
     *            The factoid to run
     * @param channel
     *            The name of the channel that the factoid is being run at
     * @param sender
     *            The nickname of the user that is running the factoid
     * @param arguments
     *            The arguments to the factoid
     * @param allowRestricted
     *            True to allow restricted factoids
     * @param vars
     *            The local variables to use. Extra variables will be added for function
     *            arguments and stuff like %channel%.
     * @return The output of the factoid, or an error message containing a pastebin url if
     *         the factoid threw an exception while running
     */
    
    public static String safeRunFactoid(Factoid f, Server server, String serverName,
            String channel, UserMessenger sender, Messenger source, String[] arguments,
            boolean allowRestricted, Map<String, String> vars)
    {
        String factValue;
        try
        {
            factValue =
                    runFactoid(f, serverName, channel, sender, source, arguments, vars,
                            allowRestricted, null);
        }
        catch (FactoidException e)
        {
            factValue =
                    "Syntax exception while running factoid: "
                        + PastebinUtils.pastebinStack(e);
        }
        catch (FactTimeExceededError e)
        {
            factValue =
                    "The factoid took too long to run: " + PastebinUtils.pastebinStack(e);
        }
        catch (StackOverflowError e)
        {
            factValue =
                    "A stack overflow occurred. This probably means you have an infinitely-recursive loop in your factoid. Details: "
                        + PastebinUtils.pastebinStack(e);
        }
        catch (Exception e)
        {
            factValue =
                    "External exception while running factoid: "
                        + PastebinUtils.pastebinStack(e);
        }
        return factValue;
    }
    
    private static void doInvalidCommand(boolean pm, Server server, String serverName,
            String channel, UserMessenger sender, Messenger source)
    {
        String defaultMessage = "Huh? (pm \"help\" for more info)";
        Channel c = server.getChannel(channel);
        String notfoundFact = Configuration.getText(null, "notfound");
        if (notfoundFact == null)
            notfoundFact = "";
        if (notfoundFact.trim().equals(""))
        {
            System.out.println("No custom notfound factoid; using standard message");
            source.sendMessage(defaultMessage);
        }
        else
        {
            System.out.println("Found a custom notfound factoid");
            try
            {
                Factoid f = null;
                if (c != null)
                    f = c.getFactoid(notfoundFact);
                if (f == null)
                    f = server.getFactoid(notfoundFact);
                if (f == null)
                    f = storage.getFactoid(notfoundFact);
                if (f == null)
                    source.sendMessage(defaultMessage);
                else
                {
                    String factValue =
                            safeRunFactoid(f, server, serverName, channel, sender, source,
                                    new String[0], true, new HashMap<String, String>());
                    if (factValue.trim().equals(""))
                        factValue = "(Not-found factoid didn't output anything)";
                    sendActionOrMessage(source, factValue);
                }
            }
            catch (Throwable t)
            {
                source.sendMessage("Syntax error in not-found factoid: "
                    + PastebinUtils.pastebinStack(t));
            }
        }
        System.out.println("Finished processing for invalid command");
    }
    
    public static void onConnect(final Server datastoreServer, final String serverName)
    {
        System.out.println("running onConnect() for server " + serverName);
        final ConnectionWrapper con = getConnection(serverName);
        new Thread()
        {
            public void run()
            {
                Utils.sleep(2300);// FIXME: make this a config var
                for (Channel channel : datastoreServer.getChannels().isolate())
                {
                    if (!channel.isSuspended())
                    {
                        Utils.sleep(2300);
                        System.out.println(serverName + ": joining " + channel.getName()
                            + " on connect");
                        con.joinChannel(channel.getName());
                    }
                }
                Utils.sleep(2500);
                try
                {
                    runNotificationFactoid(serverName, datastoreServer, null, null, con
                            .getConnection().getNick(), null, null, "_onconnect",
                            new String[0], true, true);
                }
                catch (Throwable e)
                {
                    new Exception("Exception occurred while running "
                        + "_onconnect for server \"" + serverName + "\"", e)
                            .printStackTrace();
                }
                Utils.sleep(3500);
            }
        }.start();
    }
    
    public static void onDisconnect(Server datastoreServer, String serverName)
    {
        System.out.println("on disconnect");
        // This method is commented out because the connection cycle thread essentially
        // takes care of all of this for us.
        
        // FIXME: the topic map still needs to be cleared; we're not doing that here,
        // which is bad because it lets us get misinformed about a channel's topic when
        // that channel used to have a topic but lost it while we were reconnecting.
        
        // try
        // {
        // elevatedOpMap.clear();
        // elevatedSuperopList.clear();
        // channelTopics.clear();
        // proxyStorage.close();
        // synchronized (httpServers)
        // {
        // for (int port : httpServers.keySet())
        // {
        // httpServers.get(port).stopServer();
        // }
        // httpServers.clear();
        // }
        // initProxyStorage();
        // reloadRegexes();
        // loadCachedConfig();
        // }
        // catch (Exception e)
        // {
        // e.printStackTrace();
        // }
        // System.out.println("starting reconnect thread");
        // new Thread()
        // {
        //
        // public void run()
        // {
        // int attempts = 0;
        // while (true)
        // {
        // try
        // {
        // attempts++;
        // int time;
        // if (attempts < 5)
        // time = 1;
        // else if (attempts < 10)
        // time = 5;
        // else if (attempts < 20)
        // time = 15;
        // else if (attempts < 40)
        // time = 30;
        // else if (attempts < 70)
        // time = 60;
        // else if (attempts < 100)
        // time = 120;
        // else
        // time = 240;
        // // This is to make sure that we don't flood ourselves
        // // off again after we join
        // if (manualReconnect)
        // manualReconnect = false;
        // else
        // time += 30;
        // Thread.sleep(time * 1000);
        // bot.reconnect();
        // }
        // catch (Exception e)
        // {
        // e.printStackTrace();
        // continue;
        // }
        // return;
        // }
        // }
        // }.start();
    }
    
    public static void onPrivateMessage(Server datastoreServer, String serverName,
            String sender, String login, String hostname, String message)
    {
        TimedKillThread tkt = new TimedKillThread(Thread.currentThread());
        tkt.start();
        ConnectionWrapper con = getConnection(serverName);
        if (Configuration.getBool(null, "thegame"))
        {
            String[] phrases = Configuration.getText(null, "gametext").split("\\|");
            for (String phrase : phrases)
            {
                if (message.contains(phrase))
                {
                    con.sendAction(sender, "just lost the game");
                    break;
                }
            }
        }
        String userSetScope = null;
        synchronized (pmUserScopeLock)
        {
            userSetScope = pmUserScopeMap.get("@" + serverName + "!" + sender);
            if (userSetScope != null)
            {
                pmUserScopeTimes.put("@" + serverName + "!" + sender,
                        System.currentTimeMillis());
            }
        }
        try
        {
            ChannelScope pseudoTarget = new ChannelScope(serverName, null);
            boolean replyToPseudo = false;
            boolean explicitScope = false;
            System.out.println("pm from " + sender + ": " + message);
            if (message.contains(" ") && isSuperop(serverName, hostname))
            {
                if (message.startsWith("%"))
                {
                    message = message.substring(1);
                    replyToPseudo = true;
                }
                if (message.startsWith("@") || message.startsWith("#"))
                {
                    pseudoTarget = parseFragment(message, pseudoTarget);
                    message = message.substring(message.indexOf(" ") + 1);
                    explicitScope = true;
                }
            }
            if (!explicitScope)
            {
                if (userSetScope != null)
                    pseudoTarget = parseFragment(userSetScope + " ", pseudoTarget);
            }
            if (replyToPseudo && pseudoTarget.getChannelName() == null)
            {
                con.sendMessage(sender, "You can only instruct the bot to reply to "
                    + "the target if the target contains a channel.");
                return;
            }
            try
            {
                Server pseudoDatastoreServer =
                        storage.getServer(pseudoTarget.getServerName());
                if (replyToPseudo)
                {
                    runMessageCommand(pseudoDatastoreServer, pseudoTarget.getServerName(),
                            pseudoTarget.getChannelName(), false, datastoreServer,
                            serverName, sender, null, hostname, login, message, true);
                }
                else
                {
                    runMessageCommand(pseudoDatastoreServer, pseudoTarget.getServerName(),
                            pseudoTarget.getChannelName(), true, datastoreServer,
                            serverName, sender, null, hostname, login, message, true);
                }
            }
            catch (Throwable e)
            {
                e.printStackTrace();
                con.sendMessage(sender, "Internal upper-propegating pm exception: "
                    + PastebinUtils.pastebinStack(e) + " -- " + e.getClass().getName()
                    + ": " + e.getMessage());
            }
        }
        catch (FactTimeExceededError e)
        {
            con.sendMessage(sender, "Time exceeded: " + PastebinUtils.pastebinStack(e));
        }
        finally
        {
            tkt.active = false;
        }
        
    }
    
    public static ChannelScope parseFragment(String message, ChannelScope scope)
    {
        String pseudoServer = scope.getServerName();
        String pseudoChannel = scope.getChannelName();
        if (message.startsWith("#"))
        {
            pseudoChannel = message.substring(0, message.indexOf(" "));
            message = message.substring(message.indexOf(" ") + 1);
        }
        else if (message.startsWith("@"))
        {
            String fragment = message.substring(0, message.indexOf(" "));
            message = message.substring(message.indexOf(" ") + 1);
            if (fragment.contains("#"))
            {
                pseudoServer = fragment.substring(1, fragment.indexOf("#"));
                pseudoChannel = fragment.substring(fragment.indexOf("#"));
            }
            else
            {
                pseudoServer = fragment.substring(1);
                pseudoChannel = null;
            }
        }
        return new ChannelScope(pseudoServer, pseudoChannel);
    }
    
    private static ArrayList<String> elevatedSuperopList = new ArrayList<String>();
    
    public static void elevate(String serverName, String hostname, String channel)
    {
        elevatedSuperopList.add(serverName + "@" + hostname);
    }
    
    public static boolean isSuperop(String serverName, String hostname)
    {
        if (elevatedSuperopList.contains(serverName + "@" + hostname))
            return true;
        Server server = storage.getServer(serverName);
        if (server == null)
            return false;
        return server.getOperator(hostname) != null;
    }
    
    public static void verifySuperop(String server, String hostname)
    {
        if (!isSuperop(server, hostname))
            throw new ResponseException(
                    "You are not a superop, so you don't have permission to run this command.");
    }
    
    public static User getUser(Connection con, String channel, String nick)
    {
        User[] users = con.getUsers(channel);
        for (User u : users)
        {
            if (nick.equalsIgnoreCase(u.getNick()))
                return u;
        }
        return null;
    }
    
    public static String evaluateEquation(String toEval, String channel)
    {
        return evaluateEquation(toEval, channel, "jeval");
    }
    
    public static String evaluateEquation(String toEval, String channel, String engineName)
    {
        try
        {
            Evaluator engine = getEvalEngine(engineName);
            return engine.evaluate(toEval);
        }
        catch (Exception e)
        {
            throw new FactoidException("Exception while evaluating " + toEval
                + " with engine " + engineName, e);
        }
    }
    
    /**
     * Returns a string representing this double rounded to 8 decimal points, and with no
     * decimal point if one is not needed.
     * 
     * @param value
     *            The value to round
     * @return The value
     */
    public static String toRoundedString(double value)
    {
        BigDecimal d = new BigDecimal(value);
        d = d.movePointRight(9);
        d = new BigDecimal(d.toBigInteger());
        d = d.movePointLeft(9);
        d = d.stripTrailingZeros();
        if (d.doubleValue() == 0)
            return "0";
        return d.toPlainString();
    }
    
    public static String toString(BigDecimal result)
    {
        result = result.round(jw.jzbot.eval.jeval.Operator.defaultContext);
        if (result.signum() == 0)
            return "0";
        return result.toPlainString();
    }
    
    public static Factoid getChannelFactoid(Server server, String channelName,
            String factoid)
    {
        Channel channel = server.getChannel(channelName);
        if (channel == null)
            throw new ResponseException("No such channel: " + channel);
        return channel.getFactoid(factoid);
    }
    
    public static Factoid getServerFactoid(Server server, String factoid)
    {
        return server.getFactoid(factoid);
    }
    
    public static Factoid getGlobalFactoid(String factoid)
    {
        return storage.getFactoid(factoid);
    }
    
    private static ThreadLocal<String> threadLocalUsername = new ThreadLocal<String>();
    
    public static String getThreadLocalUsername()
    {
        return threadLocalUsername.get();
    }
    
    public static void setCurrentCharset(String charset)
    {
        try
        {
            Charset.forName(charset);
        }
        catch (Exception e)
        {
            throw new ResponseException(
                    "That is not a charset supported on this platform.", e);
        }
        validateWorkableCharset(charset);
        try
        {
            synchronized (connectionCycleLock)
            {
                for (ConnectionContext con : connectionMap.values())
                    con.getConnection().setEncoding(charset);
            }
        }
        catch (Exception e)
        {
            throw new ResponseException(
                    "That is not a charset supported on this platform. "
                        + "(in bot.setEncoding(String))", e);
        }
    }
    
    private static void validateWorkableCharset(String charset)
    {
        byte[] bytes = new byte[] { '~', 's', 'o', 'm', 'e', 't', 'h', 'i', 'n', 'g' };
        try
        {
            String s = new String(bytes, charset);
            if (s.length() != 10)
                throw new ResponseException("Charset failed length validation");
            if (s.charAt(0) != '~')
                throw new ResponseException("Charset failed prefix validation");
            if (s.charAt(4) != 'e')
                throw new ResponseException("Charset failed content validation");
        }
        catch (UnsupportedEncodingException e)
        {
            throw new ResponseException(
                    "That is not a charset supported on this platform. "
                        + "(in new String(byte[],String))", e);
        }
    }
    
    /**
     * Maps server names to a map that maps channel names to a list of all regexes at the
     * channel.
     */
    private static Map<String, Map<String, List<String>>> regexCache =
            new HashMap<String, Map<String, List<String>>>();
    
    private static Object regexLock = new Object();
    
    public static final long startedAtTime = System.currentTimeMillis();
    // Easter egg
    public static final String PART_MESSAGE = "So long, and thanks for all the fish.";
    
    public static void reloadRegexes()
    {
        synchronized (regexLock)
        {
            regexCache.clear();
            for (Server server : storage.getServers().isolate())
            {
                Map<String, List<String>> thisServerMap =
                        new HashMap<String, List<String>>();
                regexCache.put(server.getName(), thisServerMap);
                for (Channel c : server.getChannels().isolate())
                {
                    ArrayList<String> list = new ArrayList<String>();
                    thisServerMap.put(c.getName(), list);
                    for (Regex regex : c.getRegularExpressions().isolate())
                    {
                        list.add(regex.getExpression());
                    }
                }
            }
        }
    }
    
    public static boolean isChannelOp(String serverName, String channel, String sender)
    {
        User[] users = getRealConnection(serverName).getConnection().getUsers(channel);
        for (User user : users)
        {
            if (user.getNick().equals(sender))
            {
                if (user.isOp())
                    return true;
            }
        }
        return false;
    }
    
    public static void logEvent(String server, String channel, String event, String nick,
            String details)
    {
        if (server.startsWith("@"))
            server = server.substring(1);
        LogEvent e = new LogEvent();
        e.server = server;
        e.channel = channel;
        e.event = event;
        e.nick = nick;
        e.details = details;
        synchronized (logQueueLock)
        {
            logQueue.offer(e);
        }
    }
    
    public static void startLogSinkThread()
    {
        Thread thread = new Thread("log-sink-thread")
        {
            public void run()
            {
                while (isRunning && logQueueRunning)
                {
                    try
                    {
                        Thread.sleep(Configuration.getInt(null, "lqdelay") * 1000);
                        System.out.println("Sinking log events...");
                        int events = 0;
                        LogEvent event;
                        synchronized (logQueueLock)
                        {
                            if (logQueueRunning)
                            {
                                while ((event = logQueue.poll()) != null)
                                {
                                    events++;
                                    sinkQueuedLogEvent(event);
                                }
                            }
                        }
                        System.out.println("Sunk " + events + " events.");
                    }
                    catch (Exception e)
                    {
                        e.printStackTrace();
                    }
                }
            }
        };
        thread.setDaemon(true);
        thread.start();
    }
    
    public static void sinkQueuedLogEvent(LogEvent logEvent)
    {
        // FIXME: This needs to be re-done using an embedded H2 database
        // String server = logEvent.server;
        // String channel = logEvent.channel;
        // String nick = logEvent.nick;
        // String details = logEvent.details;
        // String event = logEvent.event;
        // String filename = "@" + server + channel;
        // details = details.replace("\r", "").replace("\n", "");
        // try
        // {
        // // FIXME: Have this registered as a channel-specific config variable for every
        // // channel
        // // if (StringUtils.isMemberOf(filename, configNolog.split("\\|")))
        // // return;
        // String data =
        // event + " " + System.currentTimeMillis() + " " + nick + " " + details;
        // File logFile = new File(logsFolder, filename);
        // if (!logFile.exists())
        // if (!logFile.createNewFile())
        // throw new Exception("Couldn't create new log file.");
        // String content = StringUtils.readFile(logFile);
        // if ((!content.endsWith("\n")) && content.length() > 0)
        // content += "\n";
        // content += data;
        // while (content.length() > configLogsize)
        // {
        // if (content.length() == 0)
        // break;
        // int newlinePlace = content.indexOf('\n');
        // if (newlinePlace == -1)
        // newlinePlace = content.length() - 1;
        // content = content.substring(newlinePlace + 1);
        // }
        // StringUtils.writeFile(content, logFile);
        // }
        // catch (Exception e)
        // {
        // new Exception("Exception while writing data to channel logs", e)
        // .printStackTrace();
        // }
    }
    
    public static File[] listLocalFactpackFiles()
    {
        File[] files = new File("factpacks").listFiles(new FileFilter()
        {
            
            @Override
            public boolean accept(File file)
            {
                return file.getName().endsWith(".jzf");
            }
        });
        Arrays.sort(files, new Comparator<File>()
        {
            
            @Override
            public int compare(File o1, File o2)
            {
                return o1.getName().compareToIgnoreCase(o2.getName());
            }
        });
        return files;
    }
    
    public static File getLocalFactpackFile(String canonicalName)
    {
        for (File file : listLocalFactpackFiles())
        {
            Factpack pack;
            try
            {
                pack = Factpack.parse(StringUtils.readFile(file));
            }
            catch (Exception e)
            {
                throw new RuntimeException("Exception while processing file "
                    + file.getAbsolutePath(), e);
            }
            if (pack.name.equals(canonicalName))
                return file;
        }
        return null;
    }
    
    public static MathContext datasizeContext = new MathContext(3);
    
    public static BigDecimal kilo = new BigDecimal("1024");
    
    public static BigDecimal one = new BigDecimal("1");
    
    /**
     * Formats the specified data size, in bytes, to be suffixed with MB, GB, KB, etc.
     * 
     * @param size
     * @return
     */
    public static String datasize(long size)
    {
        BigDecimal number = new BigDecimal("" + size);
        String suffix = "B";
        if (number.divide(kilo).compareTo(one) >= 0)
        {
            suffix = "KB";
            number = number.divide(kilo);
            if (number.divide(kilo).compareTo(one) >= 0)
            {
                suffix = "MB";
                number = number.divide(kilo);
                if (number.divide(kilo).compareTo(one) >= 0)
                {
                    suffix = "GB";
                    number = number.divide(kilo);
                    if (number.divide(kilo).compareTo(one) >= 0)
                    {
                        suffix = "TB";
                        number = number.divide(kilo);
                    }
                }
            }
        }
        number = number.round(datasizeContext);
        return "" + number + suffix;
    }
    
    public static boolean isValidFactoidName(String factoidName)
    {
        return factoidName.matches("^[^\\@\\#\\%].*$");
    }
    
    public static String getDefaultPartMessage(String serverName, String channel)
    {
        return "Later everyone.";
    }
    
    public static String failsafeExtractServerName(String string)
    {
        try
        {
            return extractServerName(string);
        }
        catch (Exception e)
        {
            e.printStackTrace();
            return "Exception while getting server via JZBot.failsafeExtractServerName("
                + string + ")";
        }
    }
    
    public static String failsafeExtractChannelName(String string)
    {
        try
        {
            return extractChannelName(string);
        }
        catch (Exception e)
        {
            e.printStackTrace();
            return "Exception while getting channel via JZBot.failsafeExtractChannelName("
                + string + ")";
        }
    }
    
    /**
     * Returns getConnection(server).
     * 
     * @param server
     * @return
     */
    public static ConnectionWrapper getServer(String server)
    {
        return getConnection(server);
    }
    
    public static ConnectionWrapper getCheckedConnection(String serverName)
    {
        ConnectionWrapper con = getConnection(serverName);
        if (con == null)
            throw new FactoidException("There is no server for the server name "
                + serverName + ", but this command requires a valid server name.");
        return con;
    }
    
    private static Thread restartThread = new Thread("jzbot-restart-thread")
    {
        public void run()
        {
            while (isRunning)
            {
                Utils.sleep(5000);
                if (restartFile.exists())
                {
                    restartFile.delete();
                    System.out.println("RESTART FILE FOUND, RESTART IMMINENT");
                    Utils.sleep(3000);
                    restart();
                }
            }
        }
    };
    
    private static void startAutomaticRestartThread()
    {
        restartFile.delete();
        restartThread.setDaemon(true);
        restartThread.start();
    }
    
    public static void restart()
    {
        shutdownOrRestart(true);
    }
    
    public static void shutdown()
    {
        shutdownOrRestart(false);
    }
    
    public static void shutdownOrRestart(final boolean restart)
    {
        shouldRestartOnShutdown = restart;
        System.out.println(restart ? "Restarting..." : "Shutting down...");
        isRunning = false;
        int exitStatus = restart ? 17 : 0;
        try
        {
            try
            {
                new Thread()
                {
                    public void run()
                    {
                        onShutdownOrRestartGlobalDisconnect(restart);
                    }
                }.start();
            }
            catch (Throwable e)
            {
                /*
                 * This code can actually cause an exception. I just had it happen, in
                 * fact: if the compiled code is updated in such a way that the anonymous
                 * inner class representing the new thread created above is modified or
                 * renamed, a NoClassDefFoundError or NoSuchMethodError will result.
                 */
                e.printStackTrace();
            }
            Utils.sleep(5000);
            try
            {
                System.out.println("Shutting down the ProxyStorage system...");
                proxyStorage.close();
                System.out.println("ProxyStorage shut down successfully.");
            }
            catch (Throwable ex)
            {
                ex.printStackTrace();
            }
            Utils.sleep(1000);
            try
            {
                System.out.println("Shutting down the relational data store...");
                relationalStore.close();
                System.out.println("Relational store shut down successfully.");
            }
            catch (Throwable ex)
            {
                ex.printStackTrace();
            }
            Utils.sleep(2000);
            System.out.println("Exiting on " + (restart ? "restart" : "shutdown")
                + " with status " + exitStatus + "...");
            Utils.sleep(200);
            /*
             * FIXME: due to a current bug that I haven't been able to figure out,
             * replacing this with System.exit(exitStatus) causes a hang, where not even
             * Ctrl+C will kill the bot. So I'm using halt instead, after cleaning
             * everything up. This really should be changed, but it's the best I can think
             * of for now. We need, however, to consider that we need to get plugins
             * unloaded, and I'm not completely sure that all of the shutdown hooks up to
             * this point will take care of that.
             */}
        catch (Throwable e)
        {
            e.printStackTrace();
        }
        Runtime.getRuntime().halt(exitStatus);
    }
    
    protected static void onShutdownOrRestartGlobalDisconnect(final boolean isRestarting)
    {
        System.out.println("Starting global disconnect...");
        for (final ConnectionContext context : new ArrayList<ConnectionContext>(
                connectionMap.values()))
        {
            new Thread()
            {
                public void run()
                {
                    try
                    {
                        System.out.println("Disconnecting server "
                            + context.getServerName() + " on shutdown...");
                        context.getConnection().disconnect(
                                (isRestarting ? "Restarting... " : "Shutting down... ")
                                    + PART_MESSAGE);
                        System.out.println("Server " + context.getServerName()
                            + " disconnected successfully.");
                    }
                    catch (Exception ex)
                    {
                        ex.printStackTrace();
                    }
                }
            }.start();
        }
        System.out.println("Global disconnect finished.");
    }
    
    public static void proxyTraceConfigChanged()
    {
        proxyStorage.setTracingEnabled(Configuration.getBool(null, "proxytrace"));
    }
    
    public static StorageContainer getStorageContainer(String scope)
    {
        if (scope.equals("") || scope.equals("@"))
            return storage;
        if (scope.startsWith("@") && !scope.contains("#"))
            return storage.getServer(extractServerName(scope));
        if (scope.startsWith("@") && scope.contains("#"))
        {
            Server s = storage.getServer(extractServerName(scope));
            if (s == null)
                return null;
            return s.getChannel(extractChannelName(scope));
        }
        throw new IllegalArgumentException(scope + " is not a correctly-formatted scope.");
    }
    
    public static StorageContainer getCheckedStorageContainer(String scope)
    {
        StorageContainer container = getStorageContainer(scope);
        if (container == null)
            throw new IllegalStateException("The scope " + scope
                + " does not currently exist.");
        return container;
    }
}
