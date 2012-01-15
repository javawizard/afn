package com.googlecode.jwutils.timer;

import java.awt.AWTException;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.Insets;
import java.awt.MenuItem;
import java.awt.PopupMenu;
import java.awt.RenderingHints;
import java.awt.SystemTray;
import java.awt.Toolkit;
import java.awt.TrayIcon;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowFocusListener;
import java.awt.image.BufferedImage;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Scanner;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import javax.swing.BoxLayout;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSeparator;
import javax.swing.border.EmptyBorder;

import com.googlecode.jwutils.timer.Timer.Direction;

/**
 * A program that shows a tray icon. This tray icon allows for counting-up timers and
 * counting-down timers to be used, and the program will show a "Time's up" window when a
 * counting down timer finishes. When any timers are running, the tray icon shows a circle
 * with a line that rotates in it, kind of like a clock hand rotating on a clock. When no
 * timers are running, the clock is frozen in place.<br/> <br/>
 * 
 * When you click on the tray icon, a swing dialog that closes when focus is lost shows
 * up, like when you click on the clock in Windows Vista. This has two columns in it. The
 * right one shows to rows, labeled "up" and "down". They have text fields for hours,
 * minutes, and seconds, and a button to create it. The left column has currently-created
 * timers in it. When a timer is created, it adds it to this. This is in a scroll pane.
 * There is a button for pausing the timer, and there is a button for cancelling it.
 * Upbound timers never stop until they are canceled, and downbound timers are deleted
 * when time is up (and a new window is created that says time's up).
 * 
 * When you create a timer, you can enter it's name. This is shown on the time's up
 * window, and also in the left pane.
 * 
 * @author Alexander Boyd
 * 
 */
public class TrayTimer
{
    private static ArrayList<Timer> timers = new ArrayList<Timer>();
    
    private static JPanel currentTimersPanel;
    
    private static ThreadPoolExecutor threadPool = new ThreadPoolExecutor(3, 10, 10,
            TimeUnit.SECONDS, new ArrayBlockingQueue<Runnable>(100));
    
    private static TrayTimerDialog dialog;
    
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
     * @throws AWTException
     */
    public static void main(String[] args) throws AWTException
    {
        threadPool.allowCoreThreadTimeOut(true);
        dialog = new TrayTimerDialog(null);
        dialog.setUndecorated(true);
        dialog.addWindowFocusListener(new WindowFocusListener()
        {
            
            public void windowGainedFocus(WindowEvent e)
            {
                // TODO Auto-generated method stub
                
            }
            
            public void windowLostFocus(WindowEvent e)
            {
                dialog.dispose();
            }
        });
        currentTimersPanel = dialog.getCurrentTimerPanel();
        timerThread.start();
        dialog.getUpGo().addActionListener(new ActionListener()
        {
            
            public void actionPerformed(ActionEvent e)
            {
                String name = dialog.getUpName().getText();
                if (name.trim().equals(""))
                    name = generateName();
                addTimer(dialog.getUpHours().getText(), dialog.getUpMinutes().getText(),
                        dialog.getUpSeconds().getText(), true, name, true);
                dialog.getUpHours().setText("0");
                dialog.getUpMinutes().setText("0");
                dialog.getUpSeconds().setText("0");
                dialog.getUpName().setText("");
            }
        });
        dialog.getDownGo().addActionListener(new ActionListener()
        {
            
            public void actionPerformed(ActionEvent e)
            {
                String name = dialog.getDownName().getText();
                if (name.trim().equals(""))
                    name = generateName();
                addTimer(dialog.getDownHours().getText(),
                        dialog.getDownMinutes().getText(), dialog.getDownSeconds()
                                .getText(), false, name, true);
                dialog.getDownHours().setText("0");
                dialog.getDownMinutes().setText("0");
                dialog.getDownSeconds().setText("0");
                dialog.getDownName().setText("");
            }
        });
        SystemTray tray = SystemTray.getSystemTray();
        BufferedImage image = new BufferedImage(64, 64, BufferedImage.TYPE_INT_ARGB);
        Graphics2D g = image.createGraphics();
        for (int x = 0; x < image.getWidth(); x++)
        {
            for (int y = 0; y < image.getHeight(); y++)
            {
                image.setRGB(x, y, new Color(0, 0, 0, 0).getRGB());
            }
        }
        g.setColor(Color.BLACK);
        g.drawOval(0, 0, 63, 63);
        g.setColor(Color.WHITE);
        g.fillOval(4, 4, 55, 55);
        TrayIcon icon = new TrayIcon(image.getScaledInstance(16, 16,
                Image.SCALE_AREA_AVERAGING), "TrayTimer - click to show timers");
        icon.addMouseListener(new MouseListener()
        {
            
            public void mouseClicked(MouseEvent e)
            {
                if (!e.isPopupTrigger())
                    showTrayDialog();
            }
            
            public void mouseEntered(MouseEvent e)
            {
                // TODO Auto-generated method stub
                
            }
            
            public void mouseExited(MouseEvent e)
            {
                // TODO Auto-generated method stub
                
            }
            
            public void mousePressed(MouseEvent e)
            {
                // TODO Auto-generated method stub
                
            }
            
            public void mouseReleased(MouseEvent e)
            {
                // TODO Auto-generated method stub
                
            }
        });
        PopupMenu menu = new PopupMenu();
        MenuItem exit = new MenuItem("Exit");
        menu.add(exit);
        exit.addActionListener(new ActionListener()
        {
            
            public void actionPerformed(ActionEvent e)
            {
                JFrame f = new JFrame("TrayTimer");
                f.setLocationRelativeTo(null);
                f.show();
                if (JOptionPane.showConfirmDialog(f,
                        "Are you sure you want to exit TrayTimer?", null,
                        JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION)
                {
                    System.exit(0);
                }
                else
                {
                    f.dispose();
                }
            }
        });
        icon.setPopupMenu(menu);
        tray.add(icon);
        startSocketListener();
    }
    
    private static void startSocketListener()
    {
        final ServerSocket ss;
        try
        {
            ss = new ServerSocket(44728, 10);
        }
        catch (Exception e)
        {
            System.err.println("Couldn't listen on port " + 44728
                    + ". TrayTimer will run, but without remote timer support.");
            e.printStackTrace();
            return;
        }
        Thread t = new Thread()
        {
            public void run()
            {
                while (true)
                {
                    Socket s = null;
                    try
                    {
                        s = ss.accept();
                        s.setSoTimeout(25000);
                        String line = new Scanner(s.getInputStream()).nextLine();
                        processSocketLine(line);
                        s.close();
                    }
                    catch (Exception e)
                    {
                        e.printStackTrace();
                        try
                        {
                            s.close();
                        }
                        catch (Exception ex)
                        {
                            ex.printStackTrace();
                        }
                    }
                }
            }
        };
        t.setDaemon(true);
        t.start();
    }
    
    protected static void processSocketLine(String line)
    {
        String[] split = line.split("-", 6);
        addTimer(split[0], split[1], split[2], split[3].equals("up"), split[5], split[4]
                .equals("counting"));
    }
    
    protected static synchronized String generateName()
    {
        int number = 0;
        while (true)
        {
            number += 1;
            String name = "Timer " + number;
            boolean exists = false;
            for (Timer t : timers)
            {
                if (t.getName().trim().equals(name))
                {
                    exists = true;
                    break;
                }
            }
            if (exists)
                continue;
            return name;
        }
    }
    
    public static void showTrayDialog()
    {
        GraphicsEnvironment env = GraphicsEnvironment.getLocalGraphicsEnvironment();
        GraphicsDevice dv = env.getDefaultScreenDevice();
        GraphicsConfiguration cfg = dv.getDefaultConfiguration();
        Insets insets = Toolkit.getDefaultToolkit().getScreenInsets(cfg);
        Dimension size = Toolkit.getDefaultToolkit().getScreenSize();
        dialog.setLocation((size.width - insets.right) - dialog.getWidth(),
                (size.height - insets.bottom) - dialog.getHeight());
        dialog.show();
        dialog.toFront();
        dialog.show();
    }
    
    public static synchronized void doTimerUpdate()
    {
        for (Timer timer : new ArrayList<Timer>(timers))
        {
            /*
             * We need to create a new list up there since we will be modifying the list
             * to remove expired timers in this loop, which will cause a concurrent
             * modification exception
             */
            if (timer.getLabel().isSelected())
            {
                if (timer.getDirection() == Direction.UP)
                {
                    timer.setValue(timer.getValue() + 1);
                    updateTimerLabel(timer);
                }
                else
                {
                    if (timer.getValue() <= 0)
                    {
                        doTimerExpired(timer);
                    }
                    else
                    {
                        timer.setValue(timer.getValue() - 1);
                        updateTimerLabel(timer);
                    }
                }
            }
        }
    }
    
    private static synchronized void doTimerExpired(Timer timer)
    {
        final JFrame f = new JFrame("" + timer.getName() + ": Time's up");
        f.setDefaultCloseOperation(f.DISPOSE_ON_CLOSE);
        JLabel l = new JLabel("" + timer.getName()
                + (timer.getName().trim().equals("") ? "" : ": ") + "Time's up");
        l.setBorder(new EmptyBorder(20, 20, 20, 20));
        f.getContentPane().add(l);
        f.pack();
        f.setLocationRelativeTo(null);
        f.show();
        f.toFront();
        f.show();
        removeTimer(timers.indexOf(timer));
        new Thread()
        {
            public void run()
            {
                while (f.isShowing())
                {
                    try
                    {
                        Thread.sleep(1000);
                    }
                    catch (InterruptedException e)
                    {
                        e.printStackTrace();
                    }
                    Toolkit.getDefaultToolkit().beep();
                }
            }
        }.start();
    }
    
    private static void updateTimerLabel(Timer timer)
    {
        int value = timer.getValue();
        int seconds = value % 60;
        value /= 60;
        int minutes = value % 60;
        value /= 60;
        int hours = value;
        timer.getLabel().setText("" + hours + ":" + minutes + ":" + seconds);
    }
    
    public static void trayClicked()
    {
        
    }
    
    /**
     * Removes the specified timer. This involves removing it from the UI, removing it
     * from <tt>timers</tt>, and telling it not to receive events anymore.
     * 
     * @param index
     */
    public static synchronized void removeTimer(int index)
    {
        /*
         * Widgets to remove: remove index 2. Then, if there is another component that has
         * shifted to index 2 (the separator), remove it too. Then, if there isn't, and
         * there is more than one component, then remove (index 2) - 1.
         */
        currentTimersPanel.remove(index * 2);
        if (currentTimersPanel.getComponentCount() > (index * 2))
            currentTimersPanel.remove(index * 2);
        else if (currentTimersPanel.getComponentCount() > 0)
            currentTimersPanel.remove((index * 2) - 1);
        timers.remove(index);
        currentTimersPanel.invalidate();
        currentTimersPanel.validate();
        currentTimersPanel.repaint();
        dialog.getCurrentSurround().invalidate();
        dialog.getCurrentSurround().validate();
        dialog.getCurrentSurround().repaint();
    }
    
    public static synchronized void addTimer(String h, String m, String s, boolean up,
            String name, boolean counting)
    {
        TimerComponent component = new TimerComponent();
        component.getNameLabel().setText(
                "" + (up ? ((char) 8593) : ((char) 8595)) + " " + name);
        component.getMainButton().setText(h + ":" + m + ":" + s);
        component.getMainButton().setSelected(counting);
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
        if (timers.size() > 0)
            currentTimersPanel.add(new JSeparator());
        timers.add(timer);
        currentTimersPanel.add(component);
        currentTimersPanel.invalidate();
        currentTimersPanel.validate();
        currentTimersPanel.repaint();
        dialog.getCurrentSurround().invalidate();
        dialog.getCurrentSurround().validate();
        dialog.getCurrentSurround().repaint();
    }
}
