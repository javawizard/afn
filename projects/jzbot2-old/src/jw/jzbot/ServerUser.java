package jw.jzbot;

public class ServerUser implements Messenger
{
    private String serverName;
    private String nick;
    private String hostname;
    
    public String getHostname()
    {
        return hostname;
    }
    
    public void setHostname(String hostname)
    {
        this.hostname = hostname;
    }
    
    public String getServerName()
    {
        return serverName;
    }
    
    public void setServerName(String serverName)
    {
        this.serverName = serverName;
    }
    
    public String getNick()
    {
        return nick;
    }
    
    public void setNick(String nick)
    {
        this.nick = nick;
    }
    
    public ServerUser(String serverName, String nick, String hostname)
    {
        super();
        this.serverName = serverName;
        this.nick = nick;
        this.hostname = hostname;
    }
    
    public boolean isSuperop()
    {
        return JZBot.isSuperop(serverName, hostname);
    }
    
    /**
     * Throws an exception if this user is not a superop.
     */
    public void verifySuperop()
    {
        JZBot.verifySuperop(serverName, hostname);
    }
    
    public String nick()
    {
        return getNick();
    }
    
    public void sendMessage(boolean pm, String serverName, String channel, String message)
    {
        sendMessage(serverName, pm, channel, message);
    }
    
    /**
     * If <tt>pm</tt> is true, sends a message to this user. Otherwise, sends a message to
     * the specified channel on the specified server.
     * 
     * @param serverName
     * @param pm
     * @param channel
     * @param message
     */
    public void sendMessage(String serverName, boolean pm, String channel, String message)
    {
        if (pm)
        {
            ConnectionWrapper con = JZBot.getServer(this.serverName);
            con.sendMessage(this.nick, message);
        }
        else
        {
            ConnectionWrapper con = JZBot.getServer(serverName);
            con.sendMessage(channel, message);
        }
    }
    
    @Override
    public void sendMessage(String message)
    {
        ConnectionWrapper con = JZBot.getServer(serverName);
        con.sendMessage(nick, message);
    }
    
    @Override
    public int getProtocolDelimitedLength()
    {
        return JZBot.getServer(serverName).getConnection().getProtocolDelimitedLength();
    }
    
    @Override
    public void sendAction(String action)
    {
        JZBot.getConnection(serverName).sendMessage(nick, action);
    }
    
}
