package org.opengroove.jw.jmlogo.lang.commands.math;

public class ProductCommand extends TwoArgMathCommand
{
    
    public double compute(double v1, double v2)
    {
        return v1 * v2;
    }
    
    public String getName()
    {
        return "product";
    }
    
}
