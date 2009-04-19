package org.opengroove.sixjet.common.com.packets.playlist;

import org.opengroove.sixjet.common.com.Packet;

public class AddPlaylistItem extends Packet
{
    public String getPlaylist()
    {
        return playlist;
    }
    public String getItemid()
    {
        return itemid;
    }
    public String getType()
    {
        return type;
    }
    public String getValue()
    {
        return value;
    }
    public void setPlaylist(String playlist)
    {
        this.playlist = playlist;
    }
    public void setItemid(String itemid)
    {
        this.itemid = itemid;
    }
    public void setType(String type)
    {
        this.type = type;
    }
    public void setValue(String value)
    {
        this.value = value;
    }
    private String playlist;
    private String itemid;
    /**
     * Either "music" or "delay"
     */
    private String type;
    /**
     * Either the millisecond delay for a delay, or the name of some music for
     * music
     */
    private String value;
}
