package org.opengroove.sixjet.common.com.packets.playlist;

import org.opengroove.sixjet.common.com.Packet;

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
