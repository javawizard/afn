package org.opengroove.jzbot.com;

import java.net.URI;

public class ProtocolContext
{
    private String protocolName;
    
    public ProtocolContext(String protocolName)
    {
        this.protocolName = protocolName;
    }
    
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
    
    public void runExtendedEvent(URI user, URI room, String[] arguments)
    {
        
    }
}
