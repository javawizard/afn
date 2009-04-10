package tests;

import javax.swing.JDesktopPane;
import javax.swing.JFrame;

public class Test02
{
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        JFrame f = new JFrame();
        JDesktopPane d = new JDesktopPane();
        f.getContentPane().add(d);
        f.setSize(500, 400);
        f.setLocationRelativeTo(null);
        f.show();
    }
    
}
