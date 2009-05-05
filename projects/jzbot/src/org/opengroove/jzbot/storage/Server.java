package org.opengroove.jzbot.storage;

import net.sf.opengroove.common.proxystorage.ListType;
import net.sf.opengroove.common.proxystorage.Property;
import net.sf.opengroove.common.proxystorage.ProxyBean;
import net.sf.opengroove.common.proxystorage.Search;
import net.sf.opengroove.common.proxystorage.StoredList;

@ProxyBean
public interface Server
{
    /**
     * The url of this server. For example, irc://irc.freenode.net or
     * bzflag://2.bztraining.org:5167. This does not contain the port unless it
     * is different than the protocol's default port.
     * 
     * @return
     */
    @Property
    public String getUrl();
    
    public void setUrl(String url);
    
    @Property
    @ListType(Operator.class)
    public StoredList<Operator> getOperators();
    
    @Property
    @ListType(Room.class)
    public StoredList<Room> getRooms();
    
    @Property
    @ListType(Factoid.class)
    public StoredList<Factoid> getFactoids();
    
    @Search(listProperty = "factoids", searchProperty = "name", exact = true)
    public Factoid getFactoid(String name);
    
    @Property
    public boolean isSuspended();
    
    public void setSuspended(boolean suspended);
    
}
