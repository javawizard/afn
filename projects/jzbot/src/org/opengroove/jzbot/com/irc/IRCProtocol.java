package org.opengroove.jzbot.com.irc;

import java.net.URI;
import java.net.URL;
import java.util.HashMap;

import org.opengroove.jzbot.com.Protocol;
import org.opengroove.jzbot.com.ProtocolContext;
import org.opengroove.jzbot.plugins.Message;

public class IRCProtocol implements Protocol
{
    ProtocolContext context;
    /**
     * Maps server uris to bot connectors. An entry is only present if the bot
     * is connected or is supposed to be connected. IE
     * {@link #connect(URI, URI, String)} adds an entry here and
     * {@link #disconnect(URI, URI)} deletes that entry.
     */
    private HashMap<URI, IRCBot> botMap = new HashMap<URI, IRCBot>();
    
    public String allowAddOp(URI requester, URI user, URI target, boolean server)
    {
        /*
         * We're always allowed to add ops
         */
        return null;
    }
    
    public void ban(URI user, URI room, URI requester, long duration, String message)
    {
        /*
         * Not supported right now...
         */
        throw new UnsupportedOperationException();
    }
    
    public boolean banDurationAllowed()
    {
        /*
         * IRC bans are channel modes, which don't ever time out, so ban
         * durations aren't allowed
         */
        return false;
    }
    
    public boolean banMessageAllowed()
    {
        /*
         * False for now, perhaps this should be true and will send the message
         * in a pm... or maybe jzbot itself should do that automatically (IE if
         * you ban someone and they don't support a ban message, then send the
         * person that's banned a pm first with the message and then ban them)
         */
        return false;
    }
    
    public String connect(URI requester, URI server, String options)
    {
        /*
         * If we're already connected then throw some sort of exception
         */
        if(botMap.get(server))
    }
    
    public String disconnect(URI requester, URI server)
    {
        // TODO Auto-generated method stub
        return null;
    }
    
    public URI getAuthenticatedUrl(URI uri)
    {
        // TODO Auto-generated method stub
        return null;
    }
    
    public String getDisplayName(URI user)
    {
        // TODO Auto-generated method stub
        return null;
    }
    
    public String getName()
    {
        // TODO Auto-generated method stub
        return null;
    }
    
    public URI getNicknameUrl(URI uri)
    {
        // TODO Auto-generated method stub
        return null;
    }
    
    public URI getSelfUri(URI server)
    {
        // TODO Auto-generated method stub
        return null;
    }
    
    public void init(ProtocolContext context)
    {
        this.context = context;
    }
    
    public boolean isActionSupported()
    {
        // TODO Auto-generated method stub
        return false;
    }
    
    public String join(URI requester, URI room, String options)
    {
        // TODO Auto-generated method stub
        return null;
    }
    
    public void kick(URI user, URI room, URI requester)
    {
        // TODO Auto-generated method stub
        
    }
    
    public String leave(URI requester, URI room, boolean isShutdown)
    {
        // TODO Auto-generated method stub
        return null;
    }
    
    public URI normalizeServerUrl(URI uri)
    {
        // TODO Auto-generated method stub
        return null;
    }
    
    public void protocolSpecificCommand(URI source, URI user, String command)
    {
        // TODO Auto-generated method stub
        
    }
    
    public void sendMessage(URI target, Message[] messages, Message[] source)
    {
        // TODO Auto-generated method stub
        
    }
    
    public void sendRoomOpMessage(URI room, Message[] messages)
    {
        // TODO Auto-generated method stub
        
    }
    
    public void validateNewRoomOptions(URI room, String options)
    {
        // TODO Auto-generated method stub
        
    }
    
    public void validateNewServerOptions(URI server, String options)
    {
        // TODO Auto-generated method stub
        
    }
    
}
