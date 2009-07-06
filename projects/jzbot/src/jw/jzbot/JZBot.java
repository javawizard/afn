package jw.jzbot;

import java.lang.Thread.UncaughtExceptionHandler;

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
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        /*
         * 
         */
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
    }
    
    private static Scriptable globalScope;
    
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
    
}
