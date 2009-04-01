package org.opengroove.jzbot;

public interface Command
{
    public String getName();
    
    public void run(String arguments);
}
