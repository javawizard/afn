package org.opengroove.sixjet.common.com.packets.playlist;

import org.opengroove.sixjet.common.com.Packet;
import org.opengroove.sixjet.common.format.l.PlaylistFile.PlaylistItem;

/**
 * Sent from both the controller and the server. When sent from the controller,
 * the item in question includes only the type and the music or delay, not the
 * id. When sent from the server, all fields are populated.<br/>
 * <br/>
 * 
 * For each item in each playlist on the server, an instance of this packet is
 * sent to each client when it first connects.
 * 
 * @author Alexander Boyd
 * 
 */
public class AddPlaylistItem extends Packet
{
    public String getPlaylist()
    {
        return playlist;
    }
    
    public PlaylistItem getItem()
    {
        return item;
    }
    
    public void setPlaylist(String playlist)
    {
        this.playlist = playlist;
    }
    
    public void setItem(PlaylistItem item)
    {
        this.item = item;
    }
    
    private String playlist;
    private PlaylistItem item;
}
