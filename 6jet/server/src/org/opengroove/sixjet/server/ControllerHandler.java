package org.opengroove.sixjet.server;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

import net.sf.opengroove.common.security.Hash;

import org.opengroove.sixjet.common.com.Packet;
import org.opengroove.sixjet.common.com.PacketSpooler;
import org.opengroove.sixjet.common.com.StopPacket;
import org.opengroove.sixjet.common.com.packets.ChatMessage;
import org.opengroove.sixjet.common.com.packets.JetControlPacket;
import org.opengroove.sixjet.common.com.packets.NopPacket;
import org.opengroove.sixjet.common.com.packets.ServerChatMessage;
import org.opengroove.sixjet.common.com.packets.playlist.AddPlaylist;
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
    
    long token;
    
    private BlockingQueue<Packet> packetsToProcess =
        new ArrayBlockingQueue<Packet>(200);
    /*
     * This set must be a set that retains its insert ordering.
     */
    private LinkedHashSet<String> processedPacketIds = new LinkedHashSet<String>();
    
    /**
     * Schedules this inbound packet for processing. The connection thread will
     * process this packet as soon as possible.
     * 
     * @param packet
     */
    public void scheduleForProcessing(Packet packet)
    {
        packetsToProcess.add(packet);
    }
    
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
        spooler =
            new PacketSpooler(out, SixjetServer.controllerDatagramSocket, socket
                .getInetAddress(),
                SixjetServer.controllerDatagramSocket.getLocalPort() + 10, QUEUE_SIZE);
        spooler.start();
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
        }
        if (username != null)
        {
            synchronized (SixjetServer.controllerConnectionMap)
            {
                SixjetServer.controllerConnectionMap.remove(username);
                SixjetServer.controllerBroadcast(new ServerChatMessage(username
                    + " has signed off of 6jet Controller.", "Server"));
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
            loginResponse.setReason("You are already logged in as that username.");
            ControllerHandler old =
                SixjetServer.controllerConnectionMap.get(loginPacket.getUsername());
            if (old != null)
            {
                old.send(new NopPacket());
            }
        }
        if (loginResponse.isSuccessful())
        {
            /*
             * We'll generate a token now, and send it to the client.
             */
            token = Double.doubleToLongBits(Math.random());
            loginResponse.setToken(token);
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
        /*
         * Now we'll broadcast a chat message to everyone, indicating that this
         * user has signed on.
         */
        SixjetServer.synchronousControllerBroadcast(new ServerChatMessage(username
            + " has signed on to 6jet Controller.", "Server"));
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
         * Now we send any initial packets to inform the client of our state.
         */
        sendInitialState();
        /*
         * Now we'll sleep for a short time, to make sure that all udp status
         * updates get in ahead of us.
         */
        Thread.sleep(400);
        send(new ServerChatMessage("You have successfully connected. You "
            + "can now use 6jet Controller.", "Server"));
        /*
         * Now we'll start the tcp server thread.
         */
        new Thread()
        {
            public void run()
            {
                try
                {
                    while (true)
                    {
                        Packet packet = (Packet) in.readObject();
                        String packetId = packet.getPacketId();
                        boolean shouldProcess = validateAndAdd(packetId);
                        if (shouldProcess)
                            packetsToProcess.add(packet);
                    }
                }
                catch (Exception exception)
                {
                    System.err.println("Exception for user " + username);
                    if (exception instanceof IllegalStateException)
                    {
                        System.err.println("Processing queue backed up; client "
                            + "is sending commands too fast");
                    }
                    exception.printStackTrace();
                }
                try
                {
                    packetsToProcess.put(new StopPacket());
                }
                catch (InterruptedException e)
                {
                    e.printStackTrace();
                    System.err
                        .println("InterruptedException encountered while trying to stop "
                            + "controller handler. The controller thread will not "
                            + "terminate. Restart 6jet Server in order to fix this.");
                }
            }
        }.start();
        /*
         * Now we read off commands, process them, and send them on their way.
         * We don't need to check for socket connectivity in this loop since
         * we'll automatically get an exception when trying to read from the
         * socket.
         */
        while (SixjetServer.isRunning)
        {
            Packet packet = packetsToProcess.take();
            if (packet instanceof StopPacket)
                return;
            process(packet);
        }
    }
    
    /**
     * Validates that the packet specified is not in the processed packets list.
     * If it is not in the list, then it is added, and the list is trimmed back
     * to the maximum list size (currently 300).
     * 
     * @param packetId
     *            The id of the packet to check
     * @return True if this packet has not yet been processed, and should
     *         therefore be processed, or false if the packet has already been
     *         processed, and should not be processed again
     */
    public boolean validateAndAdd(String packetId)
    {
        synchronized (processedPacketIds)
        {
            boolean alreadyReceived = processedPacketIds.contains(packetId);
            if (alreadyReceived)
                return false;
            /*
             * The packet is new. We'll add it, trim the list, and return true.
             */
            processedPacketIds.add(packetId);
            /*
             * numOver is the number of elements in the list in excess of the
             * maximum number (300)
             */
            int numOver = processedPacketIds.size() - 300;
            if (numOver > 0)
            {
                Iterator iter = processedPacketIds.iterator();
                for (int i = 0; i < numOver; i++)
                {
                    iter.next();
                    iter.remove();
                }
            }
            return true;
        }
    }
    
    private void sendInitialState()
    {
        /*
         * First up is the current jet status. There is currently a potential
         * race condition here, being that we could create the jet control
         * packet, another thread could change the jet state and notify the
         * client, and then we could send this packet out. At that point, the
         * client has invalid information. This is rare enough, however, that I
         * don't see it as a real issue.
         */
        for (DescriptorFileJet jet : SixjetServer.descriptor.getJets())
        {
            JetControlPacket p =
                new JetControlPacket(jet.number, SixjetServer.getJetState(jet.number));
            send(p);
        }
        /*
         * Now we'll send this user a message for every user that's currently
         * online, telling them that the user is online.
         */
        ArrayList<ControllerHandler> handlers;
        synchronized (SixjetServer.controllerConnectionMap)
        {
            handlers =
                new ArrayList<ControllerHandler>(SixjetServer.controllerConnectionMap
                    .values());
        }
        for (ControllerHandler handler : handlers)
        {
            if (!handler.username.equals(username))
                send(new ServerChatMessage(handler.username
                    + " has signed on to 6jet Controller.", "Server"));
        }
        /*
         * Now we'll send them the list of playlists, and each playlist's
         * contents. There's another race condition here, but like the first,
         * it's not significant enough that I'm going to work on solving it
         * right now.
         */
        for (String name : SixjetServer.playlistsFolder.list())
        {
            if (!name.endsWith(""))
            {
                System.err.println("Skipping playlist file " + name
                    + " on invalid extension");
                continue;
            }
            AddPlaylist packet = new AddPlaylist();
            packet.setName(name);
            send(packet);
            /*
             * The playlist has been sent. Now we'll send its contents.
             * 
             * TODO: actually do this
             */
        }
    }
    
    private void process(Packet packet)
    {
        if (packet instanceof JetControlPacket)
            processJetControlPacket((JetControlPacket) packet);
        else if (packet instanceof NopPacket)
            processNopPacket((NopPacket) packet);
        else if (packet instanceof ChatMessage)
            processChatMessage((ChatMessage) packet);
    }
    
    private void processChatMessage(ChatMessage packet)
    {
        ServerChatMessage outPacket =
            new ServerChatMessage(packet.getMessage(), username);
        SixjetServer.controllerBroadcast(outPacket);
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
