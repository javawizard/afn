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
                        for (String key : new ArrayList<String>(state.keySet()))
                        {
                            
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
