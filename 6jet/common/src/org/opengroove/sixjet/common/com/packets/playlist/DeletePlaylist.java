package org.opengroove.sixjet.common.com.packets.playlist;

import org.opengroove.sixjet.common.com.Packet;

/**
 * Command sent by both the client and the server. This indicates that a
 * playlist is to be deleted, or that a playlist has been deleted.
 * 
 * @author Alexander Boyd
 * 
 */
public class DeletePlaylist extends Packet
{
    private String name;
    
    public String getName()
    {
        return name;
    }
    
    public void setName(String name)
    {
        this.name = name;
    }
}
