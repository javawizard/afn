package org.opengroove.sixjet.server;

import java.net.Socket;

import org.opengroove.sixjet.common.com.Packet;
import org.opengroove.sixjet.common.com.PacketSpooler;

public class ControllerHandler extends Thread
{
    private PacketSpooler spooler;
    
    private String username;
    
    /**
     * Returns the username associated with this handler, if the user has
     * successfully authenticated. If they have not authenticated, then this
     * returns null.
     * 
     * @return
     */
    public String getUsername()
    {
        return username;
    }
    
    public void send(Packet packet)
    {
        spooler.send(packet);
    }
    
    public ControllerHandler(Socket socket)
    {
        
    }
    
    public void run()
    {
        
    }
}
