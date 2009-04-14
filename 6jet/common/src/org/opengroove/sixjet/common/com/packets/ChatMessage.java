package org.opengroove.sixjet.common.com.packets;

import org.opengroove.sixjet.common.com.Packet;

public class ChatMessage extends Packet
{
    public ChatMessage(String message)
    {
        super();
        this.message = message;
        this.when = System.currentTimeMillis();
    }
    
    public String getMessage()
    {
        return message;
    }
    
    public long getWhen()
    {
        return when;
    }
    
    public void setMessage(String message)
    {
        this.message = message;
    }
    
    public void setWhen(long when)
    {
        this.when = when;
    }
    
    private String message;
    private long when;
}
