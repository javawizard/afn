package org.opengroove.sixjet.common.com.packets.playlist;

import org.opengroove.sixjet.common.com.Packet;
import org.opengroove.sixjet.common.format.l.PlaylistFile.PlaylistItem;

public class AddPlaylistItem extends Packet
{
    private String playlist;
    private PlaylistItem item;
}
