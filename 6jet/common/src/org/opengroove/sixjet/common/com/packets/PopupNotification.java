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
    
    private String message;
    
    public String getMessage()
    {
        return message;
    }
    
    public void setMessage(String message)
    {
        this.message = message;
    }
    
    public PopupNotification(String message)
    {
        super();
        this.message = message;
    }
    
}
