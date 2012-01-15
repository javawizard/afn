package jw.jzbot.fact.functions.math;

import java.util.Random;

import jw.jzbot.fact.ArgumentList;
import jw.jzbot.fact.FactContext;
import jw.jzbot.fact.Function;
import jw.jzbot.fact.Sink;

public class RandomintFunction extends Function
{
    private static Random random = new Random();
    
    @Override
    public void evaluate(Sink sink, ArgumentList arguments, FactContext context)
    {
        sink.write(random.nextInt(Integer.parseInt(arguments.resolveString(0))));
    }
    
    public String getName()
    {
        return "randomint";
    }
    
    @Override
    public String getHelp(String topic)
    {
        return "Syntax: {randomint|<number>} -- Returns a number between 0, inclusive, and "
                + "<number>, exclusive, chosen at random. The number will always be a whole "
                + "integer. If you want a number between 1 and <number>, inclusive, you could "
                + "use {eval|{random|<number>}+1} to do that.";
    }
    
}
