package org.opengroove.sixjet.common.com.packets;

import org.opengroove.sixjet.common.com.Packet;

public class ServerChatMessage extends ChatMessage
{
    public ServerChatMessage(String message, String from)
    {
        super(message);
        this.from = from;
    }
    
    public String getFrom()
    {
        return from;
    }

    public void setFrom(String from)
    {
        this.from = from;
    }

    private String from;
}
