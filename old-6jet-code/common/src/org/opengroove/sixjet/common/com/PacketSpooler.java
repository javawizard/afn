package org.opengroove.sixjet.common.com;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

public class PacketSpooler extends Thread
{
    private ObjectOutputStream out;
    private BlockingQueue<Packet> queue;
    private DatagramSocket datagramSocket;
    private InetAddress targetAddress;
    private int targetPort;
    
    public PacketSpooler(ObjectOutputStream out, DatagramSocket datagramSocket,
        InetAddress targetAddress, int targetPort, int queueSize)
    {
        this.out = out;
        this.queue = new LinkedBlockingQueue<Packet>(queueSize);
        this.datagramSocket = datagramSocket;
        this.targetAddress = targetAddress;
        this.targetPort = targetPort;
    }
    
    public synchronized boolean send(Packet packet)
    {
        boolean canOffer = queue.offer(packet);
        return canOffer;
    }
    
    private boolean closed = false;
    private final Object sendLock = new Object();
    
    public void close() throws IOException
    {
        closed = true;
        out.close();
    }
    
    public void run()
    {
        try
        {
            while (true)
            {
                Packet packet = queue.take();
                synchronized (sendLock)
                {
                    try
                    {
                        if (datagramSocket != null && packet != null)
                        {
                            ByteArrayOutputStream byteOut = new ByteArrayOutputStream();
                            ObjectOutputStream objectOut =
                                new ObjectOutputStream(byteOut);
                            objectOut.writeObject(packet);
                            objectOut.flush();
                            objectOut.close();
                            byte[] bytes = byteOut.toByteArray();
                            if (bytes.length < 32768)
                            {
                                DatagramPacket datagram =
                                    new DatagramPacket(bytes, bytes.length,
                                        targetAddress, targetPort);
                                datagramSocket.send(datagram);
                            }
                        }
                    }
                    catch (Exception exception)
                    {
                        exception.printStackTrace();
                    }
                    out.writeObject(packet);
                    out.flush();
                }
            }
        }
        catch (Exception e)
        {
            if (closed)
            {
                System.out.println("Closed packet spooler with normal close exception");
            }
            else
            {
                e.printStackTrace();
                System.out
                    .println("Closed packet spooler with the above abnormal exception");
            }
        }
    }
    
}
