package jw.sendspaced;

import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;

/**
 * A class that listens on a particular port. When a connection is received, it connects
 * to a particular host on a particular port and "links" the connections together.
 * Whenever it encounters a newline in the stream being sent to the host we're connecting
 * to, it waits 2 seconds, then continues streaming data. Right now, the port is only
 * opened on the loopback adapter.
 * 
 * @author Alexander Boyd
 * 
 */
public class SendSpaced
{
    public static final int listenPort = 44298;
    public static final String targetHost = "irc.freenode.net";
    public static final int targetPort = 6667;
    
    /**
     * @param args
     */
    public static void main(String[] args) throws Throwable
    {
        // TODO: make these configurable via program arguments
        ServerSocket ss = new ServerSocket(listenPort, 50);
        while (true)
        {
            try
            {
                Socket s = ss.accept();
                System.out.println("Connection received");
                doConnection(s);
            }
            catch (Exception e)
            {
                e.printStackTrace();
            }
        }
    }
    
    private static void doConnection(final Socket s)
    {
        new Thread()
        {
            public void run()
            {
                asyncDoConnection(s);
            }
        }.start();
    }
    
    protected static void asyncDoConnection(Socket s)
    {
        Socket target = null;
        try
        {
            target = new Socket(targetHost, targetPort);
            InputStream sourceIn = s.getInputStream();
            InputStream targetIn = target.getInputStream();
            OutputStream sourceOut = s.getOutputStream();
            OutputStream targetOut = target.getOutputStream();
            link(sourceIn, targetOut, s, target, true);
            link(targetIn, sourceOut, s, target, false);
        }
        catch (Exception e)
        {
            e.printStackTrace();
            if (target != null)
            {
                try
                {
                    target.close();
                }
                catch (Exception ex)
                {
                    ex.printStackTrace();
                }
            }
        }
        
    }
    
    private static void link(final InputStream in, final OutputStream out, final Socket s1,
            final Socket s2, final boolean newlineDelay)
    {
        Thread thread = new Thread()
        {
            public void run()
            {
                int i;
                try
                {
                    while ((i = in.read()) != -1)
                    {
                        out.write(i);
                        out.flush();
                        if (newlineDelay)
                        {
                            System.out.write(i);
                            System.out.flush();
                        }
                        else
                        {
                            System.err.write(i);
                            System.err.flush();
                        }
                        if (i == '\n')
                        {
                            if (newlineDelay)
                            {
                                // System.out.print("CLIENT: ");
                                trySleep();
                            }
                            else
                            {
                                // System.out.print("SERVER: ");
                            }
                        }
                    }
                }
                catch (Exception e)
                {
                    System.err.println("Exception with newlineDelay = " + newlineDelay);
                    e.printStackTrace();
                }
                finally
                {
                    try
                    {
                        s1.close();
                    }
                    catch (Exception ex)
                    {
                        ex.printStackTrace();
                    }
                    try
                    {
                        s2.close();
                    }
                    catch (Exception ex)
                    {
                        ex.printStackTrace();
                    }
                }
            }
        };
        thread.start();
    }
    
    protected static void trySleep()
    {
        try
        {
            Thread.sleep(2000);
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }
}
