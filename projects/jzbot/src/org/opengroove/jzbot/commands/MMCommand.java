package org.opengroove.jzbot.commands;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.opengroove.jzbot.Command;
import org.opengroove.jzbot.JZBot;
import org.opengroove.jzbot.commands.mm.MastermindState;
import org.opengroove.jzbot.commands.roulette.RouletteState;

/**
 * A game of mastermind. Uses numbers 1 through 5 as "bead colors". 4 beads by
 * default, I'll probably change that later so you can choose.
 * 
 * ~mm reset resets the game. ~mm 2435 guesses that position, which will result
 * in something like "1 right place, 2 right number wrong place", or
 * "you win! 2435 was the answer."
 * 
 * Games are reset after 10 minutes if unused.
 * 
 * @author Alexander Boyd
 * 
 */
public class MMCommand implements Command
{
    protected static final long TIME_TO_EXPIRE = 0;
    private static Map<String, MastermindState> stateMap =
        Collections.synchronizedMap(new HashMap<String, MastermindState>());
    
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
                        for (String key : new ArrayList<String>(stateMap.keySet()))
                        {
                            MastermindState value = stateMap.get(key);
                            if (value != null)
                            {
                                if ((value.changed + TIME_TO_EXPIRE) < System
                                    .currentTimeMillis())
                                    stateMap.remove(key);
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
        return "mm";
    }
    
    public void run(String channel, boolean pm, String sender, String hostname,
        String arguments)
    {
        // TODO Auto-generated method stub
        
    }
    
}
