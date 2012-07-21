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
        track = new Track(e);
        f.getContentPane().add(track);
        track.setSize(890, Mark.MARK_HEIGHT);
        track.setLocation(10, 65);
        track = new Track(e);
        f.getContentPane().add(track);
        track.setSize(890, Mark.MARK_HEIGHT);
        track.setLocation(10, 90);
        track = new Track(e);
        f.getContentPane().add(track);
        track.setSize(890, Mark.MARK_HEIGHT);
        track.setLocation(10, 115);
        track = new Track(e);
        f.getContentPane().add(track);
        track.setSize(890, Mark.MARK_HEIGHT);
        track.setLocation(10, 140);
        track = new Track(e);
        f.getContentPane().add(track);
        track.setSize(890, Mark.MARK_HEIGHT);
        track.setLocation(10, 165);
        track = new Track(e);
        f.getContentPane().add(track);
        track.setSize(890, Mark.MARK_HEIGHT);
        track.setLocation(10, 190);
        f.setSize(1000, 350);
        JPanel p = new JPanel();
        p.setLayout(new FlowLayout());
        p.setSize(600, 60);
        p.setLocation(20, 250);
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
