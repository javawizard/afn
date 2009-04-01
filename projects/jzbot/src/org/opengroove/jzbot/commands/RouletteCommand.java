package org.opengroove.jzbot.commands;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.opengroove.jzbot.Command;
import org.opengroove.jzbot.JZBot;
import org.opengroove.jzbot.commands.roulette.RouletteState;

public class RouletteCommand implements Command
{
    protected static final long TIME_TO_EXPIRE = 1000 * 60 * 5;
    private static Map<String, RouletteState> state =
        Collections.synchronizedMap(new HashMap<String, RouletteState>());
    
    static
    {
        new Thread()
        {
            public void run()
            {
                while (JZBot.isRunning)
                {
                    try
                    {
                        Thread.sleep(30 * 1000);
                        for (String key : new ArrayList<String>(state.keySet()))
                        {
                            RouletteState value = state.get(key);
                            if (value != null)
                            {
                                if ((value.changed + TIME_TO_EXPIRE) < System
                                    .currentTimeMillis())
                                    state.remove(key);
                            }
                        }
                    }
                    catch (Exception exception)
                    {
                        exception.printStackTrace();
                    }
                }
            }
        }.start();
    }
    
    public String getName()
    {
        return "roulette";
    }
    
    public void run(String arguments)
    {
        // TODO Auto-generated method stub
        
    }
    
}
