package org.opengroove.sixjet.common.com.packets;

import org.opengroove.sixjet.common.com.Packet;

/**
 * A packet that does nothing. This is used to speed up TCP timeouts when a
 * socket has been dropped, by sending a nop to a user when they try to log in
 * from elsewhere (and manage to authenticate but for the fact that they are
 * already logged in).
 * 
 * @author Alexander Boyd
 * 
 */
public class NopPacket extends Packet
{
    
}
