package org.opengroove.jzbot.storage;

import net.sf.opengroove.common.proxystorage.ListType;
import net.sf.opengroove.common.proxystorage.Property;
import net.sf.opengroove.common.proxystorage.ProxyBean;
import net.sf.opengroove.common.proxystorage.StoredList;

@ProxyBean
public interface Procedure
{
    /**
     * The name of this procedure.
     * 
     * @return
     */
    @Property
    public String getName();
    
    public void setName();
    
    /**
     * The argument line to the procedure. This could be something like
     * ":arg1 :arg2 :arg3", or the empty string if this procedure does not
     * accept arguments.
     * 
     * @return
     */
    @Property
    public String getArgumentLine();
    
    public void setArgumentLine(String argumentLine);
    
    /**
     * The contents of the procedure, excluding the "end" command.
     * 
     * @return
     */
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
    
    /**
     * A list of persistent variables. These are variables created, retrieved,
     * and deleted by use of the logo pmake, pget, and pdelete commands. These
     * variables persist across server restarts, whereas global variables do
     * not. Persistent variables are specific to a procedure. Procedures can use
     * the pexternalget command to read persistent variables of other
     * procedures, but they cannot write or delete these variables.
     * 
     * @return
     */
    @Property
    @ListType(ConfigProperty.class)
    public StoredList<ConfigProperty> getPersistentVariables();
}
