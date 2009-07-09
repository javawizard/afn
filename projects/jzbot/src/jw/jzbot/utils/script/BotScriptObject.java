package jw.jzbot.utils.script;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FilenameFilter;
import java.io.IOException;
import java.sql.Connection;

import jw.jzbot.JZBot;
import jw.jzbot.com.script.ProtocolLink;

import org.mozilla.javascript.Context;
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
        return JZBot.protocolProvider.createLink(protocol);
    }
    
    public void shutdownLink(ProtocolLink link)
    {
        JZBot.protocolProvider.shutdown(link);
    }
    
    public void sendToMaster(String text)
    {
        JZBot.masterBot.sendMessage(JZBot.masterBot.channelName, text);
    }
    
    public String[] getScripts()
    {
        return JZBot.scriptStorageFolder.list(new FilenameFilter()
        {
            
            @Override
            public boolean accept(File arg0, String arg1)
            {
                return !arg1.equals(".svn");
            }
        });
    }
    
    public void load(String script)
    {
        if (!script.matches("[^\\/\\\\\\:]+\\.js"))
            throw new RuntimeException("Invalid script name to load: " + script);
        Context context = Context.enter();
        try
        {
            context
                    .compileReader(
                            new FileReader(new File(JZBot.scriptStorageFolder,
                                    script)), script, 1, null).exec(context,
                            JZBot.globalScope);
        }
        catch (FileNotFoundException e)
        {
            throw new RuntimeException(e);
        }
        catch (IOException e)
        {
            throw new RuntimeException(e);
        }
        finally
        {
            Context.exit();
        }
    }
    
    public long getTime()
    {
        return System.currentTimeMillis();
    }
}
