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
    public String getPassword();
    
    public void setPassword(String password);
    
    @Property
    @ListType(Factoid.class)
    public StoredList<Factoid> getFactoids();
    
    @Search(listProperty = "factoids", searchProperty = "name", exact = true)
    public Factoid getFactoid(String name);
    
    @Property
    @ListType(Operator.class)
    public StoredList<Operator> getOperators();
    
    @Search(listProperty = "operators", searchProperty = "hostname")
    public Operator getOperator(String hostname);
}
