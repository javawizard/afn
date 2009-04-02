package org.opengroove.jzbot.commands;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.opengroove.jzbot.Command;
import org.opengroove.jzbot.JZBot;
import org.opengroove.jzbot.ResponseException;
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
    protected static final int numberOfBeads = 4;
    protected static final double numberOfColors = 5.0;
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
        if (channel == null)
        {
            JZBot.bot.sendMessage(sender,
                "You can only use mastermind when a channel is specified.");
            return;
        }
        MastermindState state = stateMap.get(channel);
        if (state == null && !arguments.equals("reset"))
        {
            state = new MastermindState();
            state.changed = System.currentTimeMillis();
            state.guesses = 0;
            for (int i = 0; i < numberOfBeads; i++)
            {
                state.correct.add((int) (1.0 + (Math.random() * numberOfColors)));
            }
            stateMap.put(channel, state);
            JZBot.bot.sendMessage(pm ? sender : channel,
                "A new game of Mastermind has been started. Positions: "
                    + numberOfBeads + ". Numbers: " + numberOfColors
                    + ". Guess by using ~mm 1234. "
                    + "Game will reset if unused for 10 minutes.");
            return;
        }
        if (arguments.equals("reset"))
        {
            JZBot.bot.sendMessage(channel, "The game has been cleared.");
            return;
        }
        /*
         * The arguments are a guess (or at least we'll assume so)
         */
        if (arguments.length() != state.correct.size())
        {
            throw new ResponseException("You guessed " + arguments.length()
                + " numbers. However, the correct answer has " + state.correct.size()
                + " number in it. Guess that many numbers.");
        }
    }
}
