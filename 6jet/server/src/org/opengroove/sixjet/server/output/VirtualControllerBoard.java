package org.opengroove.sixjet.server.output;

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
    
    public void flush()
    {
        // TODO Auto-generated method stub
        
    }
    
    public void setJetState(int jet, boolean state)
    {
        // TODO Auto-generated method stub
        
    }
    
}
