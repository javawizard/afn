package org.opengroove.sixjet.common.ui.jetpattern;

import java.awt.Color;
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
    
    public Track()
    {
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
    }
    
    public void mouseReleased(MouseEvent e)
    {
        down = false;
    }
    
    public void mouseDragged(MouseEvent e)
    {
        
    }
    
    public void mouseMoved(MouseEvent e)
    {
    }
}
