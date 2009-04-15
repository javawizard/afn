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
     * least-significant 8 bits of the number specified. This method also
     * inserts a delay suitable for the shift registers.
     */
    public void write(int b)
    {
        ParallelPort.writeOneByte(address, b);
        for (int i = 0; i < 500; i++)
            ;
    }
    
    /**
     * Writes the least-significant byte to the first output shift register, and
     * writes the next least significant byte to the second output shift
     * register. The third byte is also written to the third register, although
     * this shift register is currently unused in the physical 6jet controller.
     * 
     * @param value
     *            The value to write
     */
    public void shiftOut(int value)
    {
        
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
