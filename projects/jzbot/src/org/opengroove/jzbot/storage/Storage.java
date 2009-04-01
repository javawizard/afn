package org.opengroove.jzbot.storage;

import net.sf.opengroove.common.proxystorage.ListType;
import net.sf.opengroove.common.proxystorage.Property;
import net.sf.opengroove.common.proxystorage.ProxyBean;
import net.sf.opengroove.common.proxystorage.Search;
import net.sf.opengroove.common.proxystorage.StoredList;

@ProxyBean
public interface Storage
{
    @Property
    @ListType(Channel.class)
    public StoredList<Channel> getChannels();
    
    @Search(listProperty = "channels", searchProperty = "name")
    public Channel getChannel(String name);
    
    @Property
    @ListType(Factoid.class)
    public StoredList<Factoid> getFactoids();
    
    @Search(listProperty = "factoids", searchProperty = "name", exact = false, anywhere = false)
    public Factoid getFactoid(String name);
}
