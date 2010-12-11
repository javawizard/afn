package afn.homeview;

import java.awt.BorderLayout;
import java.awt.Frame;
import java.awt.GridLayout;
import java.awt.Panel;
import java.awt.ScrollPane;

public class HomeView
{
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        Frame frame = new Frame();
        Panel inScroll = new Panel();
        ScrollPane scroll = new ScrollPane(ScrollPane.SCROLLBARS_AS_NEEDED);
        scroll.add(inScroll);
        frame.add(scroll);
        inScroll.setLayout(new BorderLayout());
        Panel panel = new Panel();
        panel.setLayout(new GridLayout(0, 2));
        inScroll.add(panel, BorderLayout.NORTH);
        panel.add(new Module("A1", "Alex's Fan"));
        panel.add(new Module("A3", "Alex's Bedroom"));
        panel.add(new Module("A6", "Hallway Light"));
        panel.add(new Module("B4", "Playroom Lights"));
        panel.add(new Module("B5", "Mudroom/LR Hallway"));
        frame.setSize(240, 320);
        frame.setLocationRelativeTo(null);
        frame.show();
        frame.show();
    }
    
}
