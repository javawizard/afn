package org.opengroove.jzbot.storage;

import net.sf.opengroove.common.proxystorage.Property;
import net.sf.opengroove.common.proxystorage.ProxyBean;

@ProxyBean
public interface Channel
{
    @Property
    public String getName();
    
    public void setName(String name);
    
    @Property
    public String getJoinFactoid();
    
    public void setJoinFactoid(String name);
    
    @Property
    public String getTrigger();
    
    public void setTrigger(String trigger);
}
