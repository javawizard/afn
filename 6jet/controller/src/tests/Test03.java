package tests;

import javax.swing.JFrame;

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
        Track track = new Track();
        f.getContentPane().add(track);
        track.setSize(350, Mark.MARK_HEIGHT);
    }
    
}
