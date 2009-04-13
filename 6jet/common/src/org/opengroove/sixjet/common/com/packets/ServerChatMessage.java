package org.opengroove.sixjet.common.com.packets;

import org.opengroove.sixjet.common.com.Packet;

public class ServerChatMessage extends ChatMessage
{
    public ServerChatMessage(String message, String from)
    {
        super(message);
        this.from = from;
    }
    
    private String from;
}
