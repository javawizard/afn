package org.opengroove.jzbot.com;

import java.net.URI;

import org.opengroove.jzbot.JZBot;
import org.opengroove.jzbot.plugins.InvalidProtocolException;
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
    
    private void validate(URI... urls)
    {
        for (URI url : urls)
        {
            if (url == null)
                continue;
            if (!url.getScheme().equals(protocolName))
                throw new InvalidProtocolException("Should be " + protocolName
                    + ", was " + url.getScheme());
        }
    }
    
    /**
     * Indicates to jzbot that another user has joined the room specified.
     * 
     * @param room
     *            The room that another user has joined
     * @param user
     *            The user that joined
     */
    public void joined(URI room, URI user)
    {
        validate(room, user);
        JZBot.fromProtocolJoined(protocolName, room, user);
    }
    
    /**
     * Indicates to jzbot that a user in a room has lost room op status.
     * 
     * @param room
     *            The room
     * @param user
     *            The user that lost op status
     * @param from
     *            The user that caused the specified user to lose op status, or
     *            null if not specified
     */
    public void roomOpRemoved(URI room, URI user, URI from)
    {
        validate(room, user);
        JZBot.fromProtocolRoomOpRemoved(room, user);
    }
    
    /**
     * Indicates to jzbot that a user in a room was kicked off of the room.
     * 
     * @param room
     * @param user
     * @param from
     */
    public void kicked(URI room, URI user, URI from)
    {
        validate(room, user);
        JZBot.fromProtocolKicked(room, user, from);
    }
    
    public void left(URI room, URI user)
    {
        validate(room, user);
        JZBot.fromProtocolLeft(protocolName, room, user);
    }
    
    public void message(URI room, URI user, Message[] messages)
    {
        validate(room, user);
        JZBot.fromProtocolMessage(protocolName, room, user, messages);
    }
    
    public void runExtendedEvent(URI user, URI room, String[] arguments)
    {
        validate(user, room);
        JZBot.fromProtocolExtendedEvent(protocolName, user, room, arguments);
    }
    
    /**
     * Indicates to jzbot that it was dragged into the specified room on the
     * protocol. This basically means that, by some protocol-specific means,
     * jzbot has been forced to join the room specified. This typically occurs
     * when a private message conversation with a user has an additional user
     * added to it, turning it into a room. The bot will be marked as in the
     * room, as if ~join had been used (but join is not called on the protocol),
     * but the room will not be rejoined upon restart.
     * 
     * @param user
     *            The user responsible for the action, or null if one cannot be
     *            determined
     * @param room
     *            The room, which generally shouldn't already exist
     */
    public void draggedIntoRoom(URI user, URI room)
    {
        throw new UnsupportedOperationException();
    }
    
    /**
     * Indicates to jzbot that it was forced out of a room by some method other
     * than kicking or banning. If jzbot is kicked from a room, it will try to
     * rejoin; if jzbot is dragged out of a room, it generally means that it
     * cannot rejoin, and so it will not attempt to.
     * 
     * @param user
     *            The user that is responsible for the action, or null if one
     *            cannot be determined
     * @param room
     *            The room, which will be left by jzbot
     */
    public void draggedOutOfRoom(URI user, URI room)
    {
        throw new UnsupportedOperationException();
    }
}
