package tests;

import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;

import javax.swing.JDesktopPane;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.border.LineBorder;

public class Test02
{
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        JFrame f = new JFrame();
        final JLabel l = new JLabel("hi");
        l.setLocation(500, 50);
        l.setBorder(new LineBorder(Color.RED));
        f.getContentPane().setLayout(null);
        f.getContentPane().add(l);
        l.setSize(l.getPreferredSize());
        f.show();
        l.addMouseMotionListener(new MouseMotionListener()
        {
            
            public void mouseDragged(MouseEvent e)
            {
                System.out.println("dragged " + e.getX() + "," + e.getY());
                l.setLocation(l.getX() - 1, l.getY());
            }
            
            public void mouseMoved(MouseEvent e)
            {
                System.out.println("moved " + e.getX() + "," + e.getY());
            }
        });
    }
    
}
