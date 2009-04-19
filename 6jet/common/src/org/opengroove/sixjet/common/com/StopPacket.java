package org.opengroove.sixjet.common.com;

/**
 * A packet that is never actually sent across the wire. This is used within the
 * client and the server to indicate to various packet-processing threads that
 * the connection has been closed.
 * 
 * @author Alexander Boyd
 * 
 */
public class StopPacket extends Packet
{
    
}
