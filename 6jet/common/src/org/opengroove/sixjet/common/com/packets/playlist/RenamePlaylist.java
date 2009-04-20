package org.opengroove.sixjet.common.com.packets.playlist;

import org.opengroove.sixjet.common.com.Packet;

/**
 * Command sent by both the client and the server. It indicates that a playlist
 * is to be renamed, or that a playlist has been renamed.
 * 
 * @author Alexander Boyd
 * 
 */
public class RenamePlaylist extends Packet
{
    private String oldName;
    private String newName;
}
