package jw.jzbot;

import jw.jzbot.scripting.BotScriptClassShutter;

import org.mozilla.javascript.*;

public class BotScriptContextFactory extends ContextFactory
{
    
    // Custom Context to store execution time.
    private static class BotScriptContext extends Context
    {
        long startTime;
    }
    
    static
    {
        // Initialize GlobalFactory with custom factory
        ContextFactory.initGlobal(new BotScriptContextFactory());
    }
    
    // Override makeContext()
    protected Context makeContext()
    {
        BotScriptContext cx = new BotScriptContext();
        // Make Rhino runtime to call observeInstructionCount
        // each 10000 bytecode instructions
        cx.setClassShutter(new BotScriptClassShutter());
        cx.setMaximumInterpreterStackDepth(1024);
        cx.setOptimizationLevel(-1);
        cx.setInstructionObserverThreshold(10000);
        return cx;
    }
    
    // Override hasFeature(Context, int)
    public boolean hasFeature(Context cx, int featureIndex)
    {
        // Turn on maximum compatibility with MSIE scripts
        switch (featureIndex)
        {
            
            case Context.FEATURE_MEMBER_EXPR_AS_FUNCTION_NAME:
                return true;
                
            case Context.FEATURE_RESERVED_KEYWORD_AS_IDENTIFIER:
                return true;
                
            case Context.FEATURE_PARENT_PROTO_PROPERTIES:
                return false;
        }
        return super.hasFeature(cx, featureIndex);
    }
    
    // Override observeInstructionCount(Context, int)
    protected void observeInstructionCount(Context cx, int instructionCount)
    {
        BotScriptContext mcx = (BotScriptContext) cx;
        long currentTime = System.currentTimeMillis();
        if (currentTime - mcx.startTime > 120 * 1000)
        {
            // More then 10 seconds from Context creation time:
            // it is time to stop the script.
            // Throw Error instance to ensure that script will never
            // get control back through catch or finally.
            throw new OutOfTimeError();
        }
    }
    
    protected Object doTopCall(Callable callable, Context cx, Scriptable scope,
            Scriptable thisObj, Object[] args)
    {
        BotScriptContext mcx = (BotScriptContext) cx;
        mcx.startTime = System.currentTimeMillis();
        
        return super.doTopCall(callable, cx, scope, thisObj, args);
    }
    
    public static void load()
    {
        /*
         * Everything is done in the static initializer, so this method doesn't
         * actually need to do anything.
         */
        System.out.println("loaded bot script context factory");
    }
    
}
