package org.opengroove.sixjet.common.com;

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

public class PacketSpooler extends Thread
{
    private ObjectOutputStream out;
    private BlockingQueue<Packet> queue;
    
    public PacketSpooler(ObjectOutputStream out, int queueSize)
    {
        this.out = out;
        this.queue = new LinkedBlockingQueue<Packet>(queueSize);
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
