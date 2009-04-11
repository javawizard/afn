package org.opengroove.sixjet.server.output;

/**
 * An interface that specifies how to control a 6jet fountain. Implementations
 * of this class handle actually controlling a physical 6jet fountain.
 * Currently, the only implementation of this class designed to run an actual
 * 6jet fountain is {@link ParallelPortControllerBoard}, which controls a 6jet
 * fountain by means of a 6jet board plugged into LPT1.<br/>
 * <br/>
 * 
 * Implementations should either be thread-safe or mark all methods with the
 * synchronized modifier.
 * 
 * @author Alexander Boyd
 * 
 */
public interface ControllerBoard
{
    /**
     * Sets whether a given jet is on or off. When the controller board starts
     * up, the state of each jet is indeterminate, so the state of each jet
     * should be set by the server before anything else is done.<br/>
     * <br/>
     * 
     * If the jet number specified is not present on this controller board, then
     * this method should throw a RuntimeException.<br/>
     * <br/>
     * 
     * This should not actually change the physical jet to the new state. This
     * should only happen when {@link #flush()} is called.
     * 
     * @param jet
     *            The number of the jet to turn on. This is zero-based, and will
     *            always be less than the number returned by
     *            {@link #getNumberOfJets()}
     * @param state
     *            True to turn this jet on, false to turn this jet off
     */
    public void setJetState(int jet, boolean state);
    
    /**
     * Flushes the current state of all jets to the actual fountain. This should
     * block until the data has been sent to the fountain. For example,
     * {@link ParallelPortControllerBoard} blocks during this method while it
     * sends the data out via the parallel port, and returns once it has sent
     * all data to the parallel port.
     */
    public void flush();
}
