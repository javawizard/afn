package org.opengroove.jzbot.storage;

import net.sf.opengroove.common.proxystorage.Property;
import net.sf.opengroove.common.proxystorage.ProxyBean;

@ProxyBean
public interface StringHolder
{
    @Property
    public String getValue();
    
    public void setValue(String value);
}
