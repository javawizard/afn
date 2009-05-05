package org.opengroove.jzbot.storage;

import net.sf.opengroove.common.proxystorage.ListType;
import net.sf.opengroove.common.proxystorage.Property;
import net.sf.opengroove.common.proxystorage.ProxyBean;
import net.sf.opengroove.common.proxystorage.Search;
import net.sf.opengroove.common.proxystorage.StoredList;

@ProxyBean
public interface ProtocolSettings
{
    @Property
    public String getName();
    
    public void setName(String name);
    
    @Property
    @ListType(ConfigProperty.class)
    public StoredList<ConfigProperty> getProperties();
    
    @Search(listProperty = "properties", searchProperty = "name", exact = true, anywhere = false)
    public ConfigProperty getProperty(String name);
}
