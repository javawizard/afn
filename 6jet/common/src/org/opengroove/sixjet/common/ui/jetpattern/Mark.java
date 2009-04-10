package org.opengroove.sixjet.common.ui.jetpattern;

import java.awt.Color;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Paint;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.border.Border;

/**
 * A mark on a jet pattern editor.
 * 
 * @author Alexander Boyd
 * 
 */
public class Mark extends JComponent implements MouseListener
{
    /**
     * True if the mouse is hovering over this mark, false if it is not
     */
    private boolean hovered = false;
    /**
     * True if the mouse is pressed down and it was initially pressed within
     * this mark, false if not
     */
    private boolean mouseInDown = false;
    
    
    private JetPatternEditor editor;
    
    private static final Border markBorder =
        BorderFactory.createLineBorder(JetPatternEditorColors.markBorder, 1);
    
    Mark(JetPatternEditor editor)
    {
        this.editor = editor;
        setBorder(markBorder);
        addMouseListener(this);
    }
    
    protected void paintComponent(Graphics g)
    {
    }
    
    Paint createNormalPaint()
    {
        return shadedVertical(JetPatternEditorColors.markNormalStart,
            JetPatternEditorColors.markNormalEnd);
    }
    
    private Paint shadedVertical(Color top, Color bottom)
    {
        return new GradientPaint(0, 0, top, 0, getHeight(), bottom);
    }
    
    Paint createHoveredPaint()
    {
        return shadedVertical(JetPatternEditorColors.markNormalStart.brighter(),
            JetPatternEditorColors.markNormalEnd.brighter());
    }
    
    Paint createSelectedPaint()
    {
        return shadedVertical(JetPatternEditorColors.markSelectedStart,
            JetPatternEditorColors.markSelectedEnd);
    }
    
    Paint createSelectedHoveredPaint()
    {
        return shadedVertical(JetPatternEditorColors.markSelectedStart.brighter(),
            JetPatternEditorColors.markSelectedEnd.brighter());
    }
    
    public void mouseClicked(MouseEvent e)
    {
    }
    
    public void mouseEntered(MouseEvent e)
    {
    }
    
    public void mouseExited(MouseEvent e)
    {
    }
    
    public void mousePressed(MouseEvent e)
    {
    }
    
    public void mouseReleased(MouseEvent e)
    {
    }
    
}
