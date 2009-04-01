package org.opengroove.jzbot.storage;

import net.sf.opengroove.common.proxystorage.ListType;
import net.sf.opengroove.common.proxystorage.Property;
import net.sf.opengroove.common.proxystorage.ProxyBean;
import net.sf.opengroove.common.proxystorage.StoredList;

@ProxyBean
public interface Storage
{
    @Property
    @ListType(Channel.class)
    public StoredList<Channel> getChannels();
}
