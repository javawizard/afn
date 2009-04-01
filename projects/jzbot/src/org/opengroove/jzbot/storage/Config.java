package org.opengroove.jzbot.storage;

import net.sf.opengroove.common.proxystorage.Property;
import net.sf.opengroove.common.proxystorage.ProxyBean;

@ProxyBean
public interface Config
{
    @Property
    public String getPassword();
    
    public void setPassword(String password);
    
    @Property
    public String getNick();
    
    public void setNick(String nick);
    
    @Property
    public String getServer();
    
    public void setServer(String server);
    
    @Property
    public int getPort();
    
    public void setPort(int port);
}
