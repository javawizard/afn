package org.opengroove.jzbot.com;

import java.net.URI;

import org.opengroove.jzbot.JZBot;
import org.opengroove.jzbot.plugins.Message;
import org.opengroove.jzbot.storage.ConfigProperty;
import org.opengroove.jzbot.storage.ProtocolSettings;

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
        ProtocolSettings settings = JZBot.storage.getProtocol(protocolName);
        if (settings == null)
            return null;
        ConfigProperty prop = settings.getProperty(name);
        if (prop == null)
            return null;
        return prop.getValue();
    }
    
    public void setConfigParam(String name, String value)
    {
        ProtocolSettings settings = JZBot.storage.getProtocol(protocolName);
        if (settings == null)
        {
            settings = JZBot.storage.createProtocolSettings();
            JZBot.storage.getProtocolSettings().add(settings);
        }
        ConfigProperty prop = settings.getProperty(name);
        if (prop == null)
        {
            if (value == null)// trying to delete it and it doesn't exist
                return;
            // trying to set it and it doesn't exist
            prop = JZBot.storage.createProperty();
            // TODO: not thread safe
            settings.getProperties().add(prop);
        }
        if (value == null)
            settings.getProperties().remove(prop);
        else
            prop.setValue(value);
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
