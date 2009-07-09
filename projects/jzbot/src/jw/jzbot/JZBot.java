package jw.jzbot;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.Thread.UncaughtExceptionHandler;
import java.sql.Connection;
import java.sql.DriverManager;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Properties;
import java.util.Random;

import javax.net.ssl.HttpsURLConnection;

import jw.jzbot.com.script.ProtocolProvider;
import jw.jzbot.utils.script.BotScriptObject;
import jw.jzbot.utils.script.Pastebin;

import org.mozilla.javascript.Context;
import org.mozilla.javascript.Function;
import org.mozilla.javascript.Scriptable;
import org.mozilla.javascript.ScriptableObject;

/**
 * The main class that you run to start jzbot.
 * 
 * @author Alexander Boyd
 * 
 */
public class JZBot
{
    public static final Object javascriptLock = new Object();
    
    public static final int URL_TIMEOUT = 20 * 1000;
    
    private static final String ERROR_REPORT_PREFIX = "================================================\n"
            + "JZBot Error Report\n"
            + "================================================\n"
            + "\n"
            + "An internal error has occured. Here's the stack trace:\n\n";
    
    public static Properties masterBotProperties = new Properties();
    
    public static MasterBot masterBot;
    
    public static File persistentStorageFolder;
    public static Connection persistentStorageConnection;
    public static File scriptStorageFolder;
    
    public static ProtocolProvider protocolProvider;
    
    /**
     * @param args
     */
    public static void main(String[] args) throws Throwable
    {
        System.out.println("JZBot version 0.2, written by javawizard2539");
        doInitialSetup();
        System.out.println("Initializing...");
        HttpsURLConnection.setDefaultAllowUserInteraction(false);
        Thread
                .setDefaultUncaughtExceptionHandler(new UncaughtExceptionHandler()
                {
                    
                    @Override
                    public void uncaughtException(Thread thread,
                            Throwable caught)
                    {
                        JZBot.uncaughtException(caught);
                    }
                });
        BotScriptContextFactory.load();
        System.out.println("Loading storage...");
        /*
         * Initial setup. First we'll load the configuration for the master
         * interface. And while we're at it, we'll load the default file paths
         * as well.
         */
        persistentStorageFolder = new File("storage/persistent");
        scriptStorageFolder = new File("storage/scripts");
        masterBotProperties.load(new FileInputStream(
                "storage/masterconfig.props"));
        /*
         * Now we'll create the master interface and connect it.
         */
        System.out.println("Loading MasterBot...");
        masterBot = new MasterBot();
        masterBot.setAutoNickChange(true);
        masterBot.publicSetLogin(masterBotProperties.getProperty("nick"));
        masterBot.publicSetName(masterBotProperties.getProperty("nick"));
        masterBot.setChannelName(masterBotProperties.getProperty("room"));
        try
        {
            masterBot.connect(masterBotProperties.getProperty("server"),
                    Integer.parseInt(masterBotProperties.getProperty("port")),
                    masterBotProperties.getProperty("password"));
        }
        catch (Exception e)
        {
            System.err
                    .println("The master interface could not establish an initial connection. "
                            + "The bot will not start up until the master interface "
                            + "can establish an initial connection.");
            e.printStackTrace();
            throw new RuntimeException(e);
        }
        System.out.println("Setting up volatile storage...");
        /*
         * The master interface is up and running. Now we set up the volatile
         * storage.
         */
        Context volatileCreationContext = Context.enter();
        try
        {
            volatileStorageObject = volatileCreationContext
                    .newObject(volatileCreationContext.initStandardObjects());
        }
        finally
        {
            Context.exit();
        }
        /*
         * The volatile storage is now set up. Now we need to load the initial
         * scripts, bot script object, persistent storage, script engine, and
         * protocol provider. This, however, is also needed in the reload
         * method, so we'll do it in a separate method.
         */
        System.out.println("JZBot has loaded. Running initial scripts...");
        startupScripts(masterBot.channelName);
        System.out
                .println("Initial startup has completed. JZBot is now up and running.");
    }
    
    /**
     * This method shuts down scripts by calling the global shutdown function if
     * it exists, terminating all timers, shutting down the protocol provider,
     * closing the database connection, and discarding the global scope object.
     */
    public static void shutdownScripts(boolean reload, String sender)
            throws Exception
    {
        Object globalShutdownFunction = ScriptableObject.getProperty(
                globalScope, "shutdown");
        if (globalShutdownFunction == null)
            masterBot.sendMessage(sender,
                    "No global shutdown function present. Skipping "
                            + "to internal shutdown...");
        else if (!(globalShutdownFunction instanceof Function))
            masterBot.sendMessage(sender, "Global shutdown property is not a "
                    + "function. Skipping to internal shutdown...");
        else
        {
            Context context = Context.enter();
            try
            {
                Function f = (Function) globalShutdownFunction;
                synchronized (javascriptLock)
                {
                    f.call(context, globalScope, null, new Object[]
                    {
                        new Boolean(reload)
                    });
                }
                masterBot.sendMessage(sender,
                        "Global shutdown function completed. "
                                + "Performing internal shutdown...");
            }
            catch (Exception e)
            {
                masterBot
                        .sendMessage(
                                sender,
                                "An error occured while running the global "
                                        + "shutdown function. Internal shutdown will still be "
                                        + "attempted. Stack trace: "
                                        + pastebinStackTrace(e));
            }
            finally
            {
                Context.exit();
            }
        }
        /*
         * First, we'll terminate all periodic timers. In the future, we should
         * probably do this before the internal shutdown function to prevent
         * some problems that can occur when a timer runs after shutdown.
         */
        /*
         * Now we shut down the database connection.
         */
        try
        {
            persistentStorageConnection.close();
        }
        catch (Exception e)
        {
            masterBot.sendMessage(sender,
                    "An error occured while shutting down the database. "
                            + "Shutdown will continue. Stack trace: "
                            + pastebinStackTrace(e));
        }
        persistentStorageConnection = null;
        /*
         * Now the existing protocol connections.
         */
        try
        {
            protocolProvider.shutdownAll();
        }
        catch (Exception e)
        {
            masterBot.sendMessage(sender,
                    "An error occured while shutting down the protocol provider. "
                            + "Shutdown will continue. Stack trace: "
                            + pastebinStackTrace(e));
        }
        globalScope = null;
        masterBot.sendMessage(sender, "Shutdown was successful.");
    }
    
    /**
     * This method loads the persistent storage, the bot script object, and the
     * protocol provider. It then loads the interpreter, creating a global
     * scope, setting the bot script object on it, and instructing it to load
     * and execute init.js.
     */
    public static void startupScripts(String sender)
    {
        masterBot.sendMessage(masterBot.channelName,
                "Entering startupScripts...");
        try
        {
            /*
             * First, load the database connection.
             */
            Class.forName("org.h2.Driver");
            persistentStorageConnection = DriverManager.getConnection(
                    "jdbc:h2:"
                            + persistentStorageFolder.getAbsolutePath()
                                    .replace("\\", "/") + "/db", "sa", "");
            /*
             * Now we'll load the protocol provider.
             */
            protocolProvider = new ProtocolProvider();
            /*
             * Now we'll start constructing the script engine.
             */
            Context context = Context.enter();
            try
            {
                /*
                 * Create the scope
                 */
                globalScope = context.initStandardObjects();
                ScriptableObject
                        .putProperty(globalScope, "Global", globalScope);
                /*
                 * Set the jzbot object
                 */
                ScriptableObject.putProperty(globalScope, "jzbot", Context
                        .javaToJS(new BotScriptObject(), globalScope));
                /*
                 * That's all done, and everything's ready for scripts to begin
                 * running. Now we load init.js, if it exists, and if it doesn't
                 * we issue a warning to the master bot (but leave the script
                 * system running)
                 */
                File initScriptFile = new File(scriptStorageFolder, "init.js");
                if (!initScriptFile.exists())
                {
                    masterBot
                            .sendMessage(
                                    masterBot.channelName,
                                    "init.js does not exist, so the bot has not been "
                                            + "started. Try reload to get the bot to start again "
                                            + "once you've fixed this.");
                }
                else
                {
                    synchronized (javascriptLock)
                    {
                        context.compileReader(new FileReader(initScriptFile),
                                "init.js", 1, null).exec(context, globalScope);
                    }
                }
            }
            finally
            {
                Context.exit();
            }
        }
        catch (Throwable e)
        {
            e.printStackTrace();
            String pastebinUrl = pastebinStackTrace(e);
            masterBot.sendMessage(masterBot.channelName,
                    "An exception occured while loading scripts: http://pastebin.com/"
                            + pastebinUrl);
            throw new RuntimeException(e);
        }
        masterBot.sendMessage(masterBot.channelName, "Scripts loaded.");
    }
    
    public static String pastebinStackTrace(Throwable e)
    {
        StringWriter sw = new StringWriter();
        e.printStackTrace(new PrintWriter(sw, true));
        return Pastebin.createPost("jz_master_interface", ERROR_REPORT_PREFIX
                + sw.toString(), Pastebin.Duration.DAY, null);
    }
    
    private static void doInitialSetup() throws IOException
    {
        if (!new File("storage").exists())
        {
            System.out
                    .println("The storage folder doesn't exist. This means this is JZBot's "
                            + "first time running.");
            System.out.println("Setting up the storage folder...");
            Random random = new Random();
            File storage = new File("storage");
            storage.mkdirs();
            File scripts = new File(storage, "scripts");
            scripts.mkdirs();
            Properties masterProps = new Properties();
            masterProps.load(new FileInputStream("default-masterconfig.props"));
            for (Object s : masterProps.keySet())
            {
                String key = (String) s;
                String value = masterProps.getProperty(key);
                value = value.replace("{random}", padToPreceding(Integer
                        .toHexString(random.nextInt(Integer.MAX_VALUE - 1)), 8,
                        "0"));
                masterProps.setProperty(key, value);
            }
            masterProps.store(new FileOutputStream(new File(storage,
                    "masterconfig.props")), "generated");
            new File(storage, "persistent").mkdirs();
            System.out.println("Everything's been set up.");
            System.out.println();
        }
        else
        {
            System.out.println("JZBot has been run before.");
        }
    }
    
    private static CharSequence padToPreceding(String string, int length,
            String padding)
    {
        while (string.length() < length)
            string = padding + string;
        return string;
    }
    
    /**
     * The volatile storage object. Unlike the global scope, this is persisted
     * throughout script reloads, but unlike
     */
    public static Scriptable volatileStorageObject;
    /**
     * The global scope for scripts. This is re-created every time the scripts
     * are reloaded.
     */
    public static Scriptable globalScope;
    
    /**
     * Calls the specified script function synchronously. To prevent threading
     * issues, all calls to this method and other methods like it are
     * synchronized, so that you won't ever have two scripts running at the same
     * time. <br/><br/>
     * 
     * If <tt>scope</tt> is null, then the global scope (the one used to run the
     * initial scripts) will be used.<br/><br/>
     * 
     * If this thread already has a context, it will be used; if not, a new
     * context will be created for it.
     * 
     * @param function
     * @param arguments
     */
    public static void callScriptFunction(Function function,
            Object[] arguments, Scriptable thisObject, Scriptable scope)
    {
        synchronized (javascriptLock)
        {
            Context context = Context.enter();
            try
            {
                function.call(context, (scope == null ? globalScope : scope),
                        thisObject, arguments);
            }
            finally
            {
                Context.exit();
            }
        }
    }
    
    /**
     * Indicates that an uncaught exception has occurred. This is when an
     * exception propagates out of a script, without the script catching and
     * handling it. The current implementation pastebins the stack trace and
     * sends it to the master channel.
     * 
     * @param e
     */
    public static void uncaughtException(Throwable e)
    {
        System.err.println("TODO: implement uncaughtException. Stack trace:");
        e.printStackTrace();
    }
    
    public static boolean isMasterScriptOp(String hostname)
    {
        String scriptOpString = masterBotProperties.getProperty("scriptops");
        if (scriptOpString == null || scriptOpString.trim().equals(""))
            return false;
        if (Arrays.asList(scriptOpString.split("\\&")).contains(hostname))
            return true;
        return false;
    }
}
