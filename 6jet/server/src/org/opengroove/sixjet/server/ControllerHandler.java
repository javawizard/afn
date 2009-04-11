package org.opengroove.sixjet.server;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;

import org.opengroove.sixjet.common.com.Packet;
import org.opengroove.sixjet.common.com.PacketSpooler;
import org.opengroove.sixjet.common.com.packets.setup.LoginPacket;

public class ControllerHandler extends Thread
{
    private static final int QUEUE_SIZE = 200;
    
    private ObjectInputStream in;
    
    private ObjectOutputStream out;
    
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
        if (!spooler.send(packet))
            throw new RuntimeException("Spooler is full; couldn't send packet to "
                + username);
    }
    
    public ControllerHandler(Socket socket) throws IOException
    {
        out = new ObjectOutputStream(socket.getOutputStream());
        out.flush();
        in = new ObjectInputStream(socket.getInputStream());
        spooler = new PacketSpooler(out, QUEUE_SIZE);
    }
    
    public void run()
    {
        try
        {
            main();
        }
        catch (Exception e)
        {
            e.printStackTrace();
            try
            {
                spooler.close();
                in.close();
            }
            catch (Exception exception)
            {
                exception.printStackTrace();
            }
        }
    }
    
    private void main() throws Exception
    {
        /*
         * First thing we'll do is read a login packet. According to the 6jet
         * protocol, the first packet must be a login packet, so we don't have
         * to worry about the possible class cast exception (which will just
         * result in the connection being rejected anyway, which is what we
         * want).
         */
        LoginPacket loginPacket = (LoginPacket) in.readObject();
    }
}
