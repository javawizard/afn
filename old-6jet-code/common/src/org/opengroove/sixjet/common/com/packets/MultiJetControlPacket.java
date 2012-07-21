package org.opengroove.sixjet.common.com.packets;

import org.opengroove.sixjet.common.com.Packet;

/**
 * Not used right now, but in the future, this will indicate that all of the jet
 * changes specified should be sent at the same time to the actual fountain.
 * This is important for music playback, but isn't an issue right now since
 * playback is currently done on the server.
 * 
 * @author Alexander Boyd
 * 
 */
public class MultiJetControlPacket extends Packet
{
    private JetControlPacket[] packets;
}
