package jw.jzbot.com.script;

import java.util.ArrayList;

public class ProtocolProvider
{
    private ArrayList<ProtocolLink> links = new ArrayList<ProtocolLink>();
    
    private boolean isRunning = true;
    
    public ProtocolLink createLink(String protocol)
    {
        if (!isRunning)
            throw new RuntimeException(
                    "The protocol provider has been shut down.");
        ProtocolLink link;
        if (protocol.equals("irc"))
            link = new IRCLink();
        else if (protocol.equals("bzflag"))
            return new BZFlagLink();
        else
            throw new RuntimeException("Invalid protocol: " + protocol);
        links.add(link);
        return link;
    }
    
    public void shutdown(ProtocolLink link)
    {
        links.remove(link);
        try
        {
            link.shutdown();
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }
    
    public void shutdownAll()
    {
        isRunning = false;
        for (ProtocolLink link : new ArrayList<ProtocolLink>(links))
        {
            try
            {
                link.shutdown();
            }
            catch (Exception e)
            {
                e.printStackTrace();
            }
        }
    }
}
