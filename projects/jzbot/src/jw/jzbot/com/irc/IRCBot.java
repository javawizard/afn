package jw.jzbot.com.irc;

import jw.jzbot.com.script.IRCConnection;

import org.jibble.pircbot.PircBot;
import org.jibble.pircbot.User;

public class IRCBot extends PircBot
{
    private IRCConnection owner;
    
    public IRCBot(IRCConnection owner)
    {
        this.owner = owner;
    }
}
