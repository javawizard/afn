package org.opengroove.sixjet.common.ui.jetpattern;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;

import javax.swing.JComponent;
import javax.swing.border.LineBorder;

public class Track extends JComponent implements MouseListener, MouseMotionListener
{
    private boolean down = false;
    private boolean wasDragged = false;
    private Point locationAtMouseDown;
    private int draggingLocation = -1;
    
    private JetPatternEditor editor;
    
    public Track(JetPatternEditor editor)
    {
        this.editor = editor;
        setLayout(null);
        setBorder(new LineBorder(Color.BLACK));
        addMouseListener(this);
        addMouseMotionListener(this);
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
        locationAtMouseDown = new Point(e.getX(), e.getY());
        down = true;
        repaint();
    }
    
    public void mouseReleased(MouseEvent e)
    {
        down = false;
        if (wasDragged)
        {
            int otherPos = locationAtMouseDown.x;
            int start = Math.min(otherPos, draggingLocation);
            int end = Math.max(otherPos, draggingLocation);
            if (start != end)
            {
                if ((end - start) < 4)
                {
                    end = end + (4 - (end - start));
                }
                if (end < getWidth() && end >= 0)
                {
                    Mark mark = new Mark(editor, this);
                    add(mark);
                    mark.setSize(end - start, Mark.MARK_HEIGHT);
                    mark.setLocation(start, 0);
                }
            }
        }
        draggingLocation = -1;
        repaint();
    }
    
    public void mouseDragged(MouseEvent e)
    {
        int px = locationAtMouseDown.x;
        int py = locationAtMouseDown.y;
        int cx = e.getX();
        int cy = e.getY();
        int pxu = px + 3;
        int pxd = px - 3;
        int pyu = py + 3;
        int pyd = py - 3;
        /*
         * Only count a drag as a drag if the user first drags their mouse 3
         * pixels away from where they clicked, so minor
         * "click and drag a tiny bit"'s don't get counted as drags.
         */
        if (cx > pxu || cx < pxd || cy > pyu || cy < pyd)
        {
            wasDragged = true;
        }
        if (wasDragged)
        {
            draggingLocation = Math.max(0, Math.min(getWidth() - 1, cx));
        }
        repaint();
    }
    
    protected void paintComponent(Graphics g)
    {
        
    }
    
    public void mouseMoved(MouseEvent e)
    {
    }
}
