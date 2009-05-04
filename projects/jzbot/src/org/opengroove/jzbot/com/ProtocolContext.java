package org.opengroove.jzbot.com;

import java.net.URI;

import org.opengroove.jzbot.plugins.Message;

public class ProtocolContext
{
    private String protocolName;
    
    public ProtocolContext(String protocolName)
    {
        this.protocolName = protocolName;
    }
    
    /**
     * Gets the specified config param. Protocols can use config params to store
     * settings.
     * 
     * @param name
     * @return
     */
    public String getConfigParam(String name)
    {
        return null;
    }
    
    public void setConfigParam(String name, String value)
    {
    }
    
    public void joined(URI room, URI user)
    {
        
    }
    
    public void left(URI room, URI user)
    {
        
    }
    
    public void message(URI room, URI user, Message[] messages)
    {
        
    }
    
    public void runExtendedEvent(URI user, URI room, String[] arguments)
    {
        
    }
}
