package org.opengroove.sixjet.common.com.packets.playlist;

import org.opengroove.sixjet.common.com.Packet;
import org.opengroove.sixjet.common.format.l.PlaylistFile.PlaylistItem;

/**
 * Sent from both the controller and the server. When sent from the controller,
 * the item in question includes only 
 * 
 * @author Alexander Boyd
 * 
 */
public class AddPlaylistItem extends Packet
{
    private String playlist;
    private PlaylistItem item;
}
