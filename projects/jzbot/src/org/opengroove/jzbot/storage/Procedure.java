package org.opengroove.jzbot.storage;

import net.sf.opengroove.common.proxystorage.ListType;
import net.sf.opengroove.common.proxystorage.Property;
import net.sf.opengroove.common.proxystorage.ProxyBean;
import net.sf.opengroove.common.proxystorage.StoredList;

@ProxyBean
public interface Procedure
{
    @Property
    public String getName();
    
    public void setName();
    
    @Property
    public String getArgumentLine();
    
    public void setArgumentLine(String argumentLine);
    
    @Property
    public String getContents();
    
    public void setContents(String contents);
    
    /**
     * A list of users that are allowed to edit this procedure. This, by
     * default, contains only the person that created the procedure, and there
     * must be at least one user in this list at all times. Superops can edit
     * all procedures, even if they are not in this list.
     * 
     * @return
     */
    @Property
    @ListType(StringHolder.class)
    public StoredList<StringHolder> getEditors();
}
