package org.opengroove.sixjet.common.com.packets;

import javax.swing.JOptionPane;

import org.opengroove.sixjet.common.com.Packet;

/**
 * A packet that causes
 * {@link JOptionPane#showMessageDialog(java.awt.Component, Object)} to be
 * called in order to show the specified message over the controller. This
 * should only be sent by the server.
 * 
 * @author Alexander Boyd
 * 
 */
public class PopupNotification extends Packet
{
    
}
