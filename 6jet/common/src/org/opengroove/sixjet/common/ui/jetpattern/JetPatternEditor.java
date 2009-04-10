package org.opengroove.sixjet.common.ui.jetpattern;

import java.util.ArrayList;

import javax.swing.JComponent;

/**
 * A component that shows a list of tracks, with marks in each track. The
 * component is created with a specified length, in milliseconds, and it sets
 * its initial zoom to be 10 milliseconds per pixel.
 * 
 * The editor itself shouldn't be added to a component. Instead, the scroll pane
 * returned from getScrollPane should be used instead. This scroll pane contains
 * headers for the jet names, and a time ruler. It will also draw a
 * "current position" on the ruler, if asked to.
 * 
 * The editor draws each channel as a row. Within a channel, marks are present.
 * Marks are added by clicking and dragging in an area where there is no mark. A
 * mark can be resized on its left and right side by dragging its left and right
 * sides.
 * 
 * 
 * @author Alexander Boyd
 * 
 */
public class JetPatternEditor extends JComponent
{
    /**
     * Whether or not this pattern editor is editable.
     */
    private boolean editable = true;
    
    ArrayList<Mark> selectedMarks = new ArrayList<Mark>();
    
    public JetPatternEditor(boolean editable)
    {
        this.editable = editable;
    }
    
    public void deleteSelection()
    {
        
    }
    
    /**
     * Called by the Mark class when a mark is selected or deselected.
     */
    void markSelectionUpdated()
    {
        
    }
}
