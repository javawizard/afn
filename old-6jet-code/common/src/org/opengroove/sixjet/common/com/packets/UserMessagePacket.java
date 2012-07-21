package org.opengroove.sixjet.common.com.packets;

import org.opengroove.sixjet.common.com.Packet;

/**
 * A packet that is only sent from the server. The message specified should be
 * shown in a JOptionPane above the controller window. This is used for most
 * command errors, since most of them are directly triggered by a ui control.
 * 
 * @author Alexander Boyd
 * 
 */
public class UserMessagePacket extends Packet
{
    private String message;
}
