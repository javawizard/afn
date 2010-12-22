package jw.sendspaced;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;

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
public class PidginIrcFilter
{
    public static class ConnectInfo
    {
        public volatile String zncAccount;
        public ArrayList<String> processedChannels = new ArrayList<String>();
    }
    
    public static final int listenPort = 44297;
    public static String targetHost = "10.236.1.61";
    public static int targetPort = 6227;
    
    public static final Object pythonScriptLock = new Object();
    
    public static enum Mode
    {
        client_to_server, server_to_client
    }
    
    /**
     * @param args
     */
    public static void main(String[] args) throws Throwable
    {
        if (args.length > 0)
            targetHost = args[0];
        if (args.length > 1)
            targetPort = Integer.parseInt(args[1]);
        // TODO: make these configurable via program arguments
        ServerSocket ss =
                new ServerSocket(listenPort, 50, InetAddress.getByName("localhost"));
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
            ConnectInfo info = new ConnectInfo();
            link(sourceIn, targetOut, s, target, Mode.client_to_server, System
                    .currentTimeMillis() + 10*1000, "[C->S] ", info);
            link(targetIn, sourceOut, s, target, Mode.server_to_client, System
                    .currentTimeMillis() + 10*1000, "[S->C] ", info);
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
            final Socket s2, final Mode mode, final long timeToExpire,
            final String linePrefix, final ConnectInfo info)
    {
        Thread thread = new Thread()
        {
            public void run()
            {
                PrintWriter writer = new PrintWriter(new OutputStreamWriter(out), true);
                BufferedReader reader = new BufferedReader(new InputStreamReader(in));
                boolean hasDelayed = false;
                try
                {
                    String line;
                    while ((line = reader.readLine()) != null)
                    {
                        if (line.contains(" JOIN ") && mode == Mode.server_to_client
                            && !hasDelayed)
                        {
                            hasDelayed = true;
                            long sleepTime =
                                    Math.max(0, timeToExpire - System.currentTimeMillis());
                            System.out.println("Hit a join message; delaying for "
                                + sleepTime + " ms");
                            PidginIrcFilter.sleep(sleepTime);
                            System.out.println("Finished delaying.");
                        }
                        if (mode == Mode.client_to_server && line.startsWith("PASS ")
                            && info.zncAccount == null)
                        {
                            info.zncAccount =
                                    line.substring("PASS ".length(), line.indexOf(":"));
                            System.out.println("ZNC account is: " + info.zncAccount);
                        }
                        if (mode == Mode.server_to_client && line.matches(":[^ ]+ JOIN .*"))
                        {
                            String joinChannel = line.split(" ")[2];
                            if (joinChannel.startsWith(":"))
                                joinChannel = joinChannel.substring(1);
                            System.out.println("Inbound join on " + joinChannel);
                            if (info.processedChannels.contains(joinChannel))
                                System.out.println("Skipping script "
                                    + "run on already-joined channel");
                            else
                            {
                                info.processedChannels.add(joinChannel);
                                runJoinScript(info.zncAccount, joinChannel);
                            }
                        }
                        boolean drop = false;
                        if (mode == Mode.client_to_server)
                            if (timeToExpire > System.currentTimeMillis())
                                if (line.startsWith("JOIN"))
                                    drop = true;
                        if (mode == Mode.server_to_client
                            && line
                                    .matches(":\\*buffextras![^ ]+ .* :(jcp|javawizard)![^ ]* (joined|quit|is now known as).*"))
                            line =
                                    line.replace("jcp", "*YOU_3*").replace("javawizard",
                                            "*YOU_10*");
                        if (!drop)
                        {
                            System.out.println(linePrefix + "LINE: " + line);
                            writer.print(line + "\r\n");
                            writer.flush();
                        }
                        else
                        {
                            System.out.println(linePrefix + "DROP: " + line);
                        }
                    }
                }
                catch (Exception e)
                {
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
    
    protected static void runJoinScript(final String zncAccount, final String joinChannel)
    {
        new Thread()
        {
            public void run()
            {
                synchronized (pythonScriptLock)
                {
                    System.out.println("Running script for account " + zncAccount
                        + " and channel " + joinChannel);
                    try
                    {
                        System.out.println("Process exit code is "
                            + Runtime.getRuntime().exec(
                                    new String[] {
                                            "/home/amboyd/scripts/pidgin-irc-helper",
                                            zncAccount, joinChannel }).waitFor());
                        Thread.sleep(50);
                    }
                    catch (Exception ex)
                    {
                        ex.printStackTrace();
                    }
                    System.out.println("Script done.");
                }
            }
        }.start();
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
    
    public static void sleep(long delay)
    {
        try
        {
            Thread.sleep(delay);
        }
        catch (Exception ex)
        {
            ex.printStackTrace();
        }
    }
}
