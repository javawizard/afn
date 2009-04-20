package org.opengroove.sixjet.common.com.packets.playlist;

import org.opengroove.sixjet.common.com.Packet;

/**
 * Command sent by both the client and the server. This indicates that a
 * playlist is to be created, or that a playlist has been created. This is sent
 * once for each playlist on the server when the client first connects.
 * 
 * @author Alexander Boyd
 * 
 */
public class AddPlaylist extends Packet
{
    public String getName()
    {
        return name;
    }
    
    public void setName(String name)
    {
        this.name = name;
    }
    
    private String name;
}
