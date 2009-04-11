package org.opengroove.sixjet.server.output;

import java.io.OutputStream;
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
    
    private ArrayList<Socket> sockets = new ArrayList<Socket>();
    
    private ArrayList<Bit> bits = new ArrayList<Bit>();
    
    public synchronized void flush()
    {
        String s = "";
        for (int i = 0; i < bits.size(); i++)
        {
            Bit bit = bits.get(i);
            String ns = "" + i;
            while(ns.length() < 3)
                ns += " ";
            
        }
        for (Socket socket : new ArrayList<Socket>(sockets))
        {
            try
            {
                OutputStream o = socket.getOutputStream();
                
            }
            catch (Exception exception)
            {
                exception.printStackTrace();
            }
        }
    }
    
    public synchronized void setJetState(int jet, boolean state)
    {
        // TODO Auto-generated method stub
        
    }
    
    public void init()
    {
        // TODO Auto-generated method stub
        
    }
    
}
