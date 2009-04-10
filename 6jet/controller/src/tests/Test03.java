package tests;

import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.border.LineBorder;

import org.opengroove.sixjet.common.ui.jetpattern.JetPatternEditor;
import org.opengroove.sixjet.common.ui.jetpattern.Mark;
import org.opengroove.sixjet.common.ui.jetpattern.Track;

public class Test03
{
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        JFrame f = new JFrame();
        f.getContentPane().setLayout(null);
        final JetPatternEditor e = new JetPatternEditor(true);
        Track track = new Track(e);
        f.getContentPane().add(track);
        track.setSize(890, Mark.MARK_HEIGHT);
        track.setLocation(10, 40);
        f.setSize(1000, 300);
        JPanel p = new JPanel();
        p.setLayout(new FlowLayout());
        p.setSize(600, 100);
        p.setLocation(20, 90);
        p.setBorder(new LineBorder(Color.LIGHT_GRAY));
        f.getContentPane().add(p);
        JButton deleteMarksButton = new JButton("Delete");
        deleteMarksButton.addActionListener(new ActionListener()
        {
            
            public void actionPerformed(ActionEvent e2)
            {
                e.deleteSelection();
            }
        });
        p.add(deleteMarksButton);
        f.show();
    }
}
