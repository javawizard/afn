package jw.jzbot.utils.script;

import java.sql.Connection;

import jw.jzbot.JZBot;
import jw.jzbot.com.script.ProtocolLink;

import org.mozilla.javascript.Scriptable;
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
        return JZBot.persistentStorageConnection;
    }
    
    public Scriptable getVolatile()
    {
        return JZBot.volatileStorageObject;
    }
    
    public ProtocolLink createLink(String protocol)
    {
        return JZBot.currentProtocolProvider.createLink(protocol);
    }
    
    public void shutdownLink(ProtocolLink link)
    {
        JZBot.currentProtocolProvider.shutdown(link);
    }
    
    public void sendToMaster(String text)
    {
        JZBot.masterBot.sendMessage(JZBot.masterBot.channelName, text);
    }
}
