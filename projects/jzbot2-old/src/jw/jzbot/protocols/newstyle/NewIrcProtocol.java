package jw.jzbot.protocols.newstyle;

import jw.jzbot.protocols.Connection;
import jw.jzbot.protocols.Protocol;
import jw.jzbot.protocols.irc.IrcProtocol;

public class NewIrcProtocol implements Protocol
{
    
    @Override
    public Connection createConnection()
    {
        return new IrcProtocol();
    }
    
    @Override
    public String getName()
    {
        return "irc";
    }
    
}
