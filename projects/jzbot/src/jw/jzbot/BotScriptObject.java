package jw.jzbot;

import java.sql.Connection;

import jw.jzbot.com.script.ProtocolLink;

import org.mozilla.javascript.ScriptableObject;

/**
 * The object that is made available to scripts as the global variable
 * <tt>jzbot</tt>. It provides access to functionality of jzbot that scripts
 * need to function, such as protocols, volatile data storage, and persistent
 * data storage (via a database connection).
 * 
 * @author amboyd
 * 
 */
public class BotScriptObject
{
    public Connection getPersistent()
    {
        return null;
    }
    
    public ScriptableObject getVolatile()
    {
        return null;
    }
    
    public ProtocolLink createLink(String protocol)
    {
        return null;
    }
    
    public void shutdownLink(ProtocolLink link)
    {
        
    }
}
