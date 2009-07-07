package jw.jzbot;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.Thread.UncaughtExceptionHandler;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Properties;
import java.util.Random;

import javax.net.ssl.HttpsURLConnection;

import jw.jzbot.com.script.ProtocolProvider;

import org.mozilla.javascript.Context;
import org.mozilla.javascript.Function;
import org.mozilla.javascript.Scriptable;

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
    
    public static ProtocolProvider currentProtocolProvider;
    
    public static Properties masterBotProperties = new Properties();
    
    public static MasterBot masterBot;
    
    public static File persistentStorageFolder;
    public static File scriptStorageFolder;
    
    /**
     * @param args
     */
    public static void main(String[] args) throws Throwable
    {
        System.out.println("JZBot version 0.2, written by javawizard2539");
        doInitialSetup();
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
        masterBot = new MasterBot();
        masterBot.setAutoNickChange(true);
        masterBot.publicSetLogin(masterBotProperties.getProperty("nick"));
        masterBot.publicSetName(masterBotProperties.getProperty("nick"));
        masterBot.setChannelName(masterBotProperties.getProperty("room"));
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
