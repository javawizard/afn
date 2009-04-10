package tests;

import javax.swing.JDesktopPane;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JTable;

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
        JInternalFrame if1 = new JInternalFrame();
        if1.setResizable(true);
        if1.setMaximizable(false);
        if1.setIconifiable(false);
        if1.setClosable(false);
        if1.setTitle("");
        if1.setSize(100, 20);
        d.add(if1);
        if1.show();
        JTable t;
    }
    
}
