package com.googlecode.jwutils.timer;

import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;

import javax.swing.BoxLayout;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.border.EmptyBorder;

import com.googlecode.jwutils.timer.Timer.Direction;

/**
 * A program that shows a tray icon. This tray icon allows for counting-up
 * timers and counting-down timers to be used, and the program will show a
 * "Time's up" window when a counting down timer finishes. When any timers are
 * running, the tray icon shows a circle with a line that rotates in it, kind of
 * like a clock hand rotating on a clock. When no timers are running, the clock
 * is frozen in place.<br/>
 * <br/>
 * 
 * When you click on the tray icon, a swing dialog that closes when focus is
 * lost shows up, like when you click on the clock in Windows Vista. This has
 * two columns in it. The right one shows to rows, labeled "up" and "down". They
 * have text fields for hours, minutes, and seconds, and a button to create it.
 * The left column has currently-created timers in it. When a timer is created,
 * it adds it to this. This is in a scroll pane. There is a button for pausing
 * the timer, and there is a button for cancelling it. Upbound timers never stop
 * until they are canceled, and downbound timers are deleted when time is up
 * (and a new window is created that says time's up).
 * 
 * When you create a timer, you can enter it's name. This is shown on the time's
 * up window, and also in the left pane.
 * 
 * @author Alexander Boyd
 * 
 */
public class TrayTimer
{
    private static ArrayList<Timer> timers = new ArrayList<Timer>();
    
    private static JPanel currentTimersPanel;
    
    private static JDialog dialog;
    
    private static Thread timerThread = new Thread("timer-thread")
    {
        public void run()
        {
            while (true)
            {
                try
                {
                    Thread.sleep(1000 - (System.currentTimeMillis() % 1000));
                }
                catch (Exception exception)
                {
                    exception.printStackTrace();
                }
                doTimerUpdate();
                try
                {
                    Thread.sleep(100);
                }
                catch (Exception exception)
                {
                    exception.printStackTrace();
                }
            }
        }
    };
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        currentTimersPanel = new JPanel();
        currentTimersPanel
            .setLayout(new BoxLayout(currentTimersPanel, BoxLayout.Y_AXIS));
        
    }
    
    public static synchronized void doTimerUpdate()
    {
        for(Timer timer : timers)
        {
            if(timer.getLabel().isSelected())
            {
                
            }
        }
    }
    
    public static void trayClicked()
    {
        
    }
    
    /**
     * Removes the specified timer. This involves removing it from the UI,
     * removing it from <tt>timers</tt>, and telling it not to receive events
     * anymore.
     * 
     * @param index
     */
    public static synchronized void removeTimer(int index)
    {
        /*
         * Widgets to remove: remove index * 2. Then, if there is another
         * component that has shifted to index * 2 (the separator), remove it
         * too.
         */
        currentTimersPanel.remove(index * 2);
        if (currentTimersPanel.getComponentCount() > (index * 2))
            currentTimersPanel.remove(index * 2);
        timers.remove(index);
        currentTimersPanel.revalidate();
    }
    
    public static synchronized void addTimer(String h, String m, String s, boolean up,
        String name)
    {
        TimerComponent component = new TimerComponent();
        component.getNameLabel().setText(
            "" + (up ? ((char) 8593) : ((char) 8595)) + " " + name);
        component.getMainButton().setText(h + ":" + m + ":" + s);
        final Timer timer = new Timer();
        timer.setComponent(component);
        timer.setDirection(up ? Direction.UP : Direction.DOWN);
        timer.setLabel(component.getMainButton());
        timer.setName(name);
        int seconds = Integer.parseInt(s);
        int minutes = Integer.parseInt(m);
        int hours = Integer.parseInt(h);
        timer.setValue((hours * 60 * 60) + (minutes * 60) + seconds);
        component.getCancelButton().addActionListener(new ActionListener()
        {
            
            public void actionPerformed(ActionEvent e)
            {
                synchronized (TrayTimer.class)
                {
                    removeTimer(timers.indexOf(timer));
                }
            }
        });
        timers.add(timer);
        if (timers.size() > 0)
            currentTimersPanel.add(new JSeparator());
        currentTimersPanel.add(component);
    }
}
