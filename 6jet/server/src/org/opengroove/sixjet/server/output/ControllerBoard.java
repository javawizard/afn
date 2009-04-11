package org.opengroove.sixjet.server.output;

/**
 * An interface that specifies how to control a 6jet fountain. Implementations
 * of this class handle actually controlling a physical 6jet fountain.
 * Currently, the only implementation of this class designed to run an actual
 * 6jet fountain is {@link ParallelPortControllerBoard}, which controls a 6jet
 * fountain by means of a 6jet board plugged into LPT1.
 * 
 * @author Alexander Boyd
 * 
 */
public interface ControllerBoard
{
    /**
     * Sets whether a given jet is on or off. When the controller board starts
     * up, the state of the jets are indeterminate, so all jets should be turned
     * off.
     * 
     * @param jet
     * @param state
     */
    public void setJetState(int jet, boolean state);
    /**
     * Returns the number of jets that this 
     * @return
     */
    public int getNumberOfJets();
}
