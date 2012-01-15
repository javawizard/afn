package net.sf.opengroove.common.utils;


public class Progress
{
    private double value = 0;
    public void set(double value)
    {
        if (value < 0)
            value = 0;
        if (value > 1)
            value = 1;
        this.value = value;
    }
}
