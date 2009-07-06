package jw.jzbot;

import java.util.ArrayList;

import org.mozilla.javascript.Function;

/**
 * A class that makes managing script listeners from java code easier. It has a
 * method for adding functions to it. It then has a method for calling all
 * functions added to it sequentially, passing in the arguments specified. The
 * functions will be run on the global context, and serially.
 * 
 * @author amboyd
 * 
 */
public class FunctionNotifier
{
    private ArrayList<Function> functions = new ArrayList<Function>();
    
    public void add(Function function)
    {
        functions.add(function);
    }
    
    /**
     * Calls each function. Exceptions thrown from the functions will be sent to
     * the uncaught exception handler, instead of thrown from this method, so
     * it's guaranteed that all functions will run.<br/><br/>
     * 
     * This method creates a new array list before iterating, so it's ok for the
     * functions to cause this notifier's list of functions itself to change;
     * this will not result in a ConcurrentModificationException.
     * 
     * @param arguments
     */
    public void call(Object... arguments)
    {
        for (Function function : new ArrayList<Function>(functions))
        {
            try
            {
                JZBot.callScriptFunction(function, arguments, null, null);
            }
            catch (Throwable e)
            {
                JZBot.uncaughtException(e);
            }
        }
    }
}
