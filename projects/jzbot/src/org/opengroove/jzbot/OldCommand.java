package org.opengroove.jzbot;

public interface OldCommand
{
    public String getName();
    
    public void run(String channel, boolean pm, String sender, String hostname,
        String arguments);
}
