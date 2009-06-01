package org.opengroove.jzbot.storage;

import java.net.URI;

import net.sf.opengroove.common.proxystorage.Constructor;
import net.sf.opengroove.common.proxystorage.ListType;
import net.sf.opengroove.common.proxystorage.Property;
import net.sf.opengroove.common.proxystorage.ProxyBean;
import net.sf.opengroove.common.proxystorage.Search;
import net.sf.opengroove.common.proxystorage.StoredList;

@ProxyBean
public interface Storage
{
    @Constructor
    public Factoid createFactoid();
    
    @Constructor
    public Operator createOperator();
    
    @Constructor
    public Room createRoom();
    
    @Property
    @ListType(Server.class)
    public StoredList<Server> getServers();
    
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
    
    @Property
    @ListType(ProtocolSettings.class)
    public StoredList<ProtocolSettings> getProtocolSettings();
    
    @Search(listProperty = "protocolSettings", searchProperty = "name", exact = true, anywhere = false)
    public ProtocolSettings getProtocol(String name);
    
    @Constructor
    public ProtocolSettings createProtocolSettings();
    
    @Constructor
    public ConfigProperty createProperty();
    
    @Search(listProperty = "servers", searchProperty = "url", exact = true, anywhere = false)
    public Server getServer(URI extractServerUri);
}
