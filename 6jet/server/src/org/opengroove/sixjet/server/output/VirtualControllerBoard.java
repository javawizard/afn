package org.opengroove.sixjet.server.output;

import java.io.OutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;

/**
 * An emulated controller board that supports how ever many jets are controlled
 * with it. When started, socket connections are accepted on an unused port
 * (which is then printed to stdout).
 * 
 * @author Alexander Boyd
 * 
 */
public class VirtualControllerBoard implements ControllerBoard
{
    public class Bit
    {
        public boolean value;
    }
    
    protected static final int MAX_SOCKETS = 20;
    
    private ArrayList<Socket> sockets = new ArrayList<Socket>();
    
    private ArrayList<Bit> bits = new ArrayList<Bit>();
    
    public synchronized void flush()
    {
        String s = "";
        for (int i = 0; i < bits.size(); i++)
        {
            Bit bit = bits.get(i);
            String ns = "" + (i + 1);
            while (ns.length() < 3)
                ns += " ";
            String nu = "   ";
            if (bit.value)
                s += nu;
            else
                s += ns;
        }
        for (Socket socket : new ArrayList<Socket>(sockets))
        {
            try
            {
                OutputStream o = socket.getOutputStream();
                o.write(s.getBytes());
                o.write("\r\n".getBytes());
                o.flush();
            }
            catch (Exception exception)
            {
                exception.printStackTrace();
                try
                {
                    socket.close();
                }
                catch (Exception exception2)
                {
                    exception2.printStackTrace();
                }
                sockets.remove(socket);
            }
        }
    }
    
    public synchronized void setJetState(int jet, boolean state)
    {
        while (jet >= bits.size())
        {
            bits.add(new Bit());
        }
        Bit bit = bits.get(jet);
        bit.value = state;
    }
    
    public void init()
    {
        try
        {
            final ServerSocket ss = new ServerSocket(0);
            System.out.println("VirtualControllerBoard listening on port "
                + ss.getLocalPort());
            new Thread()
            {
                public void run()
                {
                    while (true)
                    {
                        try
                        {
                            Socket s = ss.accept();
                            if (sockets.size() > MAX_SOCKETS)
                            {
                                s.getOutputStream().write(
                                    "Too many connections\r\n".getBytes());
                                s.getOutputStream().flush();
                                s.close();
                            }
                            else
                            {
                                s.getOutputStream().write(
                                    "Connected.\r\n".getBytes());
                                s.getOutputStream().flush();
                                sockets.add(s);
                            }
                        }
                        catch (Exception exception)
                        {
                            exception.printStackTrace();
                        }
                    }
                }
            }.start();
        }
        catch (Exception e)
        {
        }
    }
}
