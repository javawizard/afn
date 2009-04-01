package org.opengroove.jzbot.storage;

import net.sf.opengroove.common.proxystorage.Property;
import net.sf.opengroove.common.proxystorage.ProxyBean;

@ProxyBean
public interface Factoid
{
    @Property
    public String getName();
    
    public void setName(String name);
    
    @Property
    public boolean isActive();
    
    public void setActive(boolean active);
    
    @Property
    public String getValue();
    
    public void setValue(String value);
}
