package org.opengroove.sixjet.server;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;

import net.sf.opengroove.common.security.Hash;

import org.opengroove.sixjet.common.com.Packet;
import org.opengroove.sixjet.common.com.PacketSpooler;
import org.opengroove.sixjet.common.com.packets.ChatMessage;
import org.opengroove.sixjet.common.com.packets.JetControlPacket;
import org.opengroove.sixjet.common.com.packets.NopPacket;
import org.opengroove.sixjet.common.com.packets.ServerChatMessage;
import org.opengroove.sixjet.common.com.packets.setup.DescriptorFilePacket;
import org.opengroove.sixjet.common.com.packets.setup.LoginPacket;
import org.opengroove.sixjet.common.com.packets.setup.LoginResponse;
import org.opengroove.sixjet.common.format.d.DescriptorFile.DescriptorFileJet;

public class ControllerHandler extends Thread
{
    private static final int QUEUE_SIZE = 200;
    
    private ObjectInputStream in;
    
    private ObjectOutputStream out;
    
    private PacketSpooler spooler;
    
    private String username;
    
    public void trySend(Packet packet)
    {
        try
        {
            send(packet);
        }
        catch (Exception exception)
        {
            exception.printStackTrace();
        }
    }
    
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
            if (username != null)
            {
                synchronized (SixjetServer.controllerConnectionMap)
                {
                    SixjetServer.controllerConnectionMap.remove(username);
                }
            }
            try
            {
                spooler.close();
                in.close();
                out.close();
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
        String correctPasswordEnc =
            SixjetServer.controllerAuthProperties
                .getProperty(loginPacket.getUsername());
        String passwordEnc = Hash.hash(loginPacket.getPassword());
        System.out.println(passwordEnc);
        System.out.println(correctPasswordEnc);
        LoginResponse loginResponse = new LoginResponse();
        loginResponse.setSuccessful(passwordEnc.equals(correctPasswordEnc));
        if (!loginResponse.isSuccessful())
        {
            loginResponse.setReason("Incorrect username/password");
        }
        if (loginResponse.isSuccessful()
            && SixjetServer.controllerConnectionMap.get(loginPacket.getUsername()) != null)
        {
            loginResponse.setSuccessful(false);
            loginResponse.setReason("You are already logged in.");
            ControllerHandler old =
                SixjetServer.controllerConnectionMap.get(loginPacket.getUsername());
            if (old != null)
            {
                old.send(new NopPacket());
            }
        }
        out.writeObject(loginResponse);
        out.flush();
        if (!loginResponse.isSuccessful())
        {
            in.close();
            out.close();
            return;
        }
        /*
         * We logged the person in successfully. Now we add them to the
         * connection map.
         */
        username = loginPacket.getUsername();
        synchronized (SixjetServer.controllerConnectionMap)
        {
            SixjetServer.controllerConnectionMap.put(username, this);
        }
        /*
         * Now we send them the descriptor. From here on out, everything will be
         * sent by way of the packet spooler.
         */
        DescriptorFilePacket descriptorPacket = new DescriptorFilePacket();
        descriptorPacket.setFile(SixjetServer.descriptor);
        send(descriptorPacket);
        /*
         * Now we send any initial packets to inform the client of our state.F
         */
        sendInitialState();
        send(new ServerChatMessage("You have successfully connected. You "
            + "can now use 6jet Controller.", "Server"));
        /*
         * Now we read off commands, process them, and send them on their way.
         * We don't need to check for socket connectivity in this loop since
         * we'll automatically get an exception when trying to read from the
         * socket.
         */
        while (SixjetServer.isRunning)
        {
            Packet packet = (Packet) in.readObject();
            process(packet);
        }
    }
    
    private void sendInitialState()
    {
        for (DescriptorFileJet jet : SixjetServer.descriptor.getJets())
        {
            JetControlPacket p =
                new JetControlPacket(jet.number, SixjetServer.getJetState(jet.number));
            send(p);
        }
    }
    
    private void process(Packet packet)
    {
        if (packet instanceof JetControlPacket)
            processJetControlPacket((JetControlPacket) packet);
        else if (packet instanceof NopPacket)
            processNopPacket((NopPacket) packet);
    }
    
    private void processNopPacket(NopPacket packet)
    {
        /*
         * Nothing to do for a nop packet
         */
    }
    
    private void processJetControlPacket(JetControlPacket packet)
    {
        /*
         * FIXME: this needs to check to see if music is playing. If music is
         * playing, then this method should do nothing.
         */
        SixjetServer.setJetState(packet.getJet(), packet.isState());
        SixjetServer.flushBoard();
    }
}
