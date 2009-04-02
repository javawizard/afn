package org.opengroove.jzbot.commands;

import org.opengroove.jzbot.Command;

/**
 * A game of mastermind. Uses numbers 1 through 5 as "bead colors". 4 beads by
 * default, I'll probably change that later so you can choose.
 * 
 * ~mm reset resets the game. ~mm 2435 guesses that position, which will result
 * in something like "1 right place, 2 right number wrong place", or
 * "you win! 2435 was the answer."
 * 
 * @author Alexander Boyd
 * 
 */
public class MMCommand implements Command
{
    
    public String getName()
    {
        // TODO Auto-generated method stub
        return null;
    }
    
    public void run(String channel, boolean pm, String sender, String hostname,
        String arguments)
    {
        // TODO Auto-generated method stub
        
    }
    
}
