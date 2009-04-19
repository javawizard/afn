package org.opengroove.sixjet.common.com;

import java.io.Serializable;

/**
 * A 6jet communication packet. Communication in 6jet is done by sending
 * subclasses of this class via object streams over tcp/ip.
 * 
 * @author Alexander Boyd
 * 
 */
public class Packet implements Serializable
{
    
    private static final long serialVersionUID = -7033775827263609295L;
    
    private String packetId;
    
    private long dateReceived;
    
    public Packet()
    {
        packetId = "" + System.currentTimeMillis() + "-" + Math.random();
    }
    
    public String getPacketId()
    {
        return packetId;
    }
}
