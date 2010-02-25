package jw.jzbot.fact.functions.mutex;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.ReentrantLock;

import jw.jzbot.fact.ArgumentList;
import jw.jzbot.fact.FactContext;
import jw.jzbot.fact.Function;
import jw.jzbot.fact.Sink;

public class SynchronizedFunction extends Function
{
    public static final Object mapLock = new Object();
    
    public static Map<String, ReentrantLock> lockMap = new HashMap<String, ReentrantLock>();
    
    @Override
    public void evaluate(Sink sink, ArgumentList arguments, FactContext context)
    {
        ReentrantLock lock = null;
        lock.lock();
        try
        {
            arguments.resolve(1, sink);
        }
        finally
        {
            lock.unlock();
        }
    }
    
    @Override
    public String getHelp(String topic)
    {
        return "Syntax: {synchronized|<name>|<code>} -- Evaluates to <code>, but "
            + "guarantees that, for a particular <name>, no two {synchronized} "
            + "blocks will run their <code> at the same time. This means that if, "
            + "in some other factoid, a {synchronized} block with a particular "
            + "<name> is currently running, and this factoid is run, and it "
            + "contains a {synchronized} block with the same <name>, this factoid "
            + "will wait until the other factoid exits its {synchronized} block "
            + "before continuing.";
    }
    
}
