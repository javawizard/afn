package org.opengroove.sixjet.common.ui;

public interface JetDisplayListener
{
    /**
     * Indicates that the specified jet has been pressed with the mouse.
     * 
     * @param jet
     *            The jet's number, according to the descriptor file
     */
    public void jetDown(int jet);
    
    /**
     * Indicates that the specified jet has been released with the mouse.
     * 
     * @param jet
     *            The jet's number, according to the descriptor file
     * @param in
     *            True if the mouse up event occured within the jet, false if
     *            the mouse has been dragged out of the jet
     */
    public void jetUp(int jet, boolean in);
}
