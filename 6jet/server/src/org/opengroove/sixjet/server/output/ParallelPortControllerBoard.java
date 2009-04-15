package org.opengroove.sixjet.server.output;

import parport.ParallelPort;

public class ParallelPortControllerBoard implements ControllerBoard
{
    /**
     * The address of the parallel port to communicate with. 0x378 (888 in
     * decimal) is LPT1. This will probably be configurable in the future.
     */
    public static final int address = 0x378;
    
    public void flush()
    {
        
    }
    
    /**
     * Sets pins 2 through 9 on the physical parallel port to be the
     * least-significant 8 bits of the number specified.
     */
    public void write(int b)
    {
        ParallelPort.writeOneByte(address, b);
    }
    
    public void setJetState(int jet, boolean state)
    {
        // TODO Auto-generated method stub
        
    }
    
    public void init()
    {
        // TODO Auto-generated method stub
        
    }
    
    public boolean getJetState(int jet)
    {
        // TODO Auto-generated method stub
        return false;
    }
    
}
