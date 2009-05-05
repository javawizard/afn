package org.opengroove.jzbot;

import org.opengroove.jzbot.com.irc.IRCProtocol;

public class Defaults
{
    
    public static void installDefaultCommands()
    {
        // TODO Auto-generated method stub
        
    }
    
    public static void installDefaultProtocols()
    {
        JZBot.installProtocol(new IRCProtocol());
    }
    
}
