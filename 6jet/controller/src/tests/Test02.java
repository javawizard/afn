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
        JLabel l = new JLabel("hi");
        l.setBorder(new LineBorder(Color.RED));
        JLabel l2 = new JLabel("bye");
        f.getContentPane().setLayout(new FlowLayout());
        f.getContentPane().add(l);
        f.getContentPane().add(l2);
        f.show();
        l.addMouseMotionListener(new MouseMotionListener()
        {
            
            public void mouseDragged(MouseEvent e)
            {
                System.out.println("dragged " + e.getX() + "," + e.getY());
            }
            
            public void mouseMoved(MouseEvent e)
            {
                System.out.println("moved " + e.getX() + "," + e.getY());
            }
        });
    }
    
}
