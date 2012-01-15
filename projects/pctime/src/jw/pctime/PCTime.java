package jw.pctime;

import java.io.InputStream;
import java.io.OutputStream;
import java.net.ServerSocket;
import java.net.Socket;

public class PCTime
{
    public static ServerSocket server;
    
    /**
     * @param args
     */
    public static void main(String[] args) throws Throwable
    {
        server = new ServerSocket(27463);
        while (true)
        {
            Socket s = server.accept();
            handleIncomingSocket(s);
        }
    }
    
    private static void handleIncomingSocket(Socket s) throws Exception
    {
        InputStream socketIn = s.getInputStream();
        OutputStream socketOut = s.getOutputStream();
        int version = socketIn.read();
        System.out.println(version);
        int authMethodCount = socketIn.read();
        System.out.println(authMethodCount);
        for (int i = 0; i < authMethodCount; i++)
        {
            int authMethod = socketIn.read();
            System.out.println("  " + authMethod);
        }
    }
    
}
