package jw.jzbot.storage;

import net.sf.opengroove.common.proxystorage.Default;
import net.sf.opengroove.common.proxystorage.Property;
import net.sf.opengroove.common.proxystorage.ProxyBean;

@ProxyBean
public interface MapEntry
{
    @Property
    public String getKey();
    
    public void setKey(String key);
    
    @Property
    @Default(stringValue = "")
    public String getValue();
    
    public void setValue(String value);
}
