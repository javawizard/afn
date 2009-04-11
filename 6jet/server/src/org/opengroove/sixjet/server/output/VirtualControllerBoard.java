package org.opengroove.sixjet.server.output;

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
    
    private ArrayList<Bit> bits = new ArrayList<Bit>();
    
    public void flush()
    {
        // TODO Auto-generated method stub
        
    }
    
    public void setJetState(int jet, boolean state)
    {
        // TODO Auto-generated method stub
        
    }
    
}
