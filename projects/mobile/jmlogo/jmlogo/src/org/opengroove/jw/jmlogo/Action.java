package org.opengroove.jw.jmlogo;

public abstract class Action
{
    private String name;
    public String getName()
    {
        return name;
    }
    
    protected Action(String name)
    {
        this.name = name;
    }
    
    public abstract void run()
    {
        
    }
}
