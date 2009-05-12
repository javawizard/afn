package com.googlecode.jwutils.timer;

import java.util.ArrayList;

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
    private ArrayList<Timer> timers = new ArrayList<Timer>();
    
    private JPanel currentTimersPanel;
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        
    }
    
    public static void trayClicked()
    {
        
    }
    
}
