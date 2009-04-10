package org.opengroove.sixjet.common.ui.jetpattern;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.GradientPaint;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;

/**
 * A mark on a jet pattern editor.
 * 
 * @author Alexander Boyd
 * 
 */
public class Mark extends JComponent implements MouseListener, MouseMotionListener
{
    /**
     * True if the mouse is hovering over this mark, false if it is not
     */
    private boolean hovered = false;
    
    private Track track;
    /**
     * True if the mouse is pressed down and it was initially pressed within
     * this mark, false if not
     */
    private boolean down = false;
    
    private boolean wasDragged = false;
    /**
     * The position of this mark, relative to its parent track, that the mark
     * was located at when the mouse was clicked.
     */
    private Point locationAtMouseDown = null;
    
    private Dimension sizeAtMouseDown = null;
    
    private enum DragTarget
    {
        LEFT, RIGHT, MARK, NONE
    }
    
    private DragTarget dragTarget = DragTarget.NONE;
    
    private JetPatternEditor editor;
    
    private static final Border markBorder =
        BorderFactory.createLineBorder(JetPatternEditorColors.markBorder, 1);
    
    public static final int MARK_HEIGHT = 25;
    
    public Mark(JetPatternEditor editor, Track track)
    {
        this.track = track;
        this.editor = editor;
        setBorder(markBorder);
        setLayout(null);
        addMouseListener(this);
        addMouseMotionListener(this);
    }
    
    protected void paintComponent(Graphics g)
    {
        Paint paint;
        boolean selected = isSelected();
        boolean hovered2 = hovered && !down;
        if (hovered2 && selected)
            paint = createSelectedHoveredPaint();
        else if (hovered2 && !selected)
            paint = createHoveredPaint();
        else if (!hovered2 && selected)
            paint = createSelectedPaint();
        else
            paint = createNormalPaint();
        Graphics2D g2 = (Graphics2D) g;
        g2.setPaint(paint);
        g2.fillRect(0, 0, getWidth(), getHeight());
    }
    
    private boolean isSelected()
    {
        if (editor != null)
            return editor.selectedMarks.contains(this);
        return false;
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
        return shadedVertical(brighter(JetPatternEditorColors.markNormalStart),
            brighter(JetPatternEditorColors.markNormalEnd));
    }
    
    private Color brighter(Color color)
    {
        return new Color(Math.min(255, (int) (color.getRed() * 1.1d)), Math.min(255,
            (int) (color.getGreen() * 1.1d)), Math.min(255,
            (int) (color.getBlue() * 1.1d)));
    }
    
    Paint createSelectedPaint()
    {
        return shadedVertical(JetPatternEditorColors.markSelectedStart,
            JetPatternEditorColors.markSelectedEnd);
    }
    
    Paint createSelectedHoveredPaint()
    {
        return shadedVertical(brighter(JetPatternEditorColors.markSelectedStart),
            brighter(JetPatternEditorColors.markSelectedEnd));
    }
    
    public void mouseClicked(MouseEvent e)
    {
    }
    
    public void mouseEntered(MouseEvent e)
    {
        hovered = true;
        repaint();
    }
    
    public void mouseExited(MouseEvent e)
    {
        hovered = false;
        repaint();
    }
    
    public void mousePressed(MouseEvent e)
    {
        down = true;
        locationAtMouseDown = getLocation();
        sizeAtMouseDown = getSize();
        int width = getWidth();
        int mx = e.getX();
        if (mx < 4)
        {
            dragTarget = DragTarget.LEFT;
        }
        else if (mx > (width - 4))
        {
            dragTarget = DragTarget.RIGHT;
        }
        else
        {
            dragTarget = DragTarget.NONE;
        }
        /*
         * DragTarget.mark will be added soon, which is the middle of the mark
         * somewhere. This shifts the mark left or right.
         */
        repaint();
    }
    
    public void mouseReleased(MouseEvent e)
    {
        down = false;
        if (wasDragged)
        {
            finalizeDrag();
        }
        repaint();
    }
    
    /**
     * Finalizes a drag operation. This sends back to the parent jet pattern
     * editor that a resize or drag has taken place, and tells it to persist the
     * drag or resize to its internal channel/channelevent model.
     */
    private void finalizeDrag()
    {
    }
    
    public void mouseDragged(MouseEvent e)
    {
        if (dragTarget == DragTarget.NONE)
            return;
        Point mouseTrackPoint =
            SwingUtilities.convertPoint(this, new Point(e.getX(), e.getY()), track);
        if (mouseTrackPoint.x < 1)
            mouseTrackPoint.x = 1;
        else if (mouseTrackPoint.x > (track.getWidth() - 1))
            mouseTrackPoint.x = track.getWidth() - 1;
        if (dragTarget == DragTarget.LEFT)
        {
            int newX = mouseTrackPoint.x;
            int newSize =
                sizeAtMouseDown.width + (locationAtMouseDown.x - mouseTrackPoint.x);
            if (newSize < 4)
            {
                newX -= (4 - newSize);
                newSize = 4;
            }
            setLocation(newX, 0);
            setSize(newSize, MARK_HEIGHT);
        }
        else if (dragTarget == DragTarget.RIGHT)
        {
            int newSize = mouseTrackPoint.x - locationAtMouseDown.x;
            if (newSize < 4)
                newSize = 4;
            setSize(newSize, MARK_HEIGHT);
        }
    }
    
    public void mouseMoved(MouseEvent e)
    {
        if (e.getX() < 4)
        {
            setCursor(Cursor.getPredefinedCursor(Cursor.W_RESIZE_CURSOR));
        }
        else if (e.getX() > (getWidth() - 4))
        {
            setCursor(Cursor.getPredefinedCursor(Cursor.E_RESIZE_CURSOR));
        }
        else
        {
            setCursor(Cursor.getDefaultCursor());
        }
    }
    
}
