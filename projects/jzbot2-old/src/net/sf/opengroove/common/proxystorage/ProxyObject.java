package net.sf.opengroove.common.proxystorage;

import java.beans.PropertyChangeListener;

import javax.swing.event.ChangeListener;

/**
 * This interface is implemented by all proxy bean objects. Interfaces annotated with
 * {@link ProxyBean} don't need to implement this interface; instances of the proxy bean
 * interface can just be cast to this one. Proxy beans can, however, implement it if they
 * wish their users to be able to use the methods in this interface without having to
 * cast.
 * 
 * @author Alexander Boyd
 * 
 */
public interface ProxyObject
{
    /**
     * Gets the id under which the object is stored in the ProxyStorage system. This is
     * unique for any given object within the proxy storage system; two objects with the
     * same id represent the same object, and changes made to one will be immediately
     * visible to the other. Ids will never be reused by the proxy storage system even
     * after an object that uses a particular id is garbage collected.
     * 
     * @return
     */
    public long getProxyStorageId();
    
    public Class getProxyStorageClass();
    
    public boolean isProxyStoragePresent();
    
    public boolean equals(Object object);
    
    public int hashCode();
    
    /**
     * Adds a listener that will be notified when the specified property changes.
     * Specifically, this listener will be notified whenever this property's setter is
     * called. If a StoredList needs to be watched for changes in it's contents, TODO: add
     * storedlist listener method on storedlist.java
     * 
     * @param listener
     *            The listener to add
     */
    public void addChangeListener(String property, PropertyChangeListener listener);
    
    /**
     * Removes a change listener previously added with
     * {@link #addChangeListener(String, ChangeListener)}.
     * 
     * @param listener
     *            The listener to remove
     */
    public void removeChangeListener(String property, PropertyChangeListener listener);
}
