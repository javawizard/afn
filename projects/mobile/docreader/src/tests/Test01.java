package tests;

import java.awt.Frame;
import java.awt.Label;
import java.awt.ScrollPane;
import java.awt.TextArea;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import jw.docreader.ui.OptionPane;

public class Test01
{
    
    /**
     * @param args
     */
    public static void main(String[] args) throws Throwable
    {
        final Frame f = new Frame("DocReader Test");
        f.setSize(240, 320);
        Label l = new Label("Randomizing...");
        f.add(l);
        f.show();
        StringBuffer message = new StringBuffer("message");
        while (message.length() < 4000)
        {
            message.append("abcdefghijklmnopqrstuvwxyz".charAt((int) (Math.random() * 26)));
        }
        l.setText("Showing in 1 second \nnewline...");
        Thread.sleep(1000);
        f.removeAll();
        final TextArea area = new TextArea(message.toString(), 0, 0,
                TextArea.SCROLLBARS_VERTICAL_ONLY);
        f.add(area);
        f.invalidate();
        f.validate();
        f.repaint();
        area.addMouseListener(new MouseAdapter()
        {
            
            @Override
            public void mouseClicked(MouseEvent e)
            {
                OptionPane.showMessageDialog(f, "Clicked " + e.getX() + " " + e.getY()
                        + " " + e.paramString() + " " + e.getModifiers());
            }
        });
    }
}
