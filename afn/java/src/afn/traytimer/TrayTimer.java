package afn.traytimer;

import info.clearthought.layout.TableLayout;

import java.awt.Color;
import java.awt.Component;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.SwingUtilities;

import afn.libautobus.AutobusConnection;
import afn.libautobus.ObjectListener;
import afn.libautobus.proxies.TimerInterface;
import afn.traytimer.ui.TimerPanel;

public class TrayTimer
{
    public static class UpdateTimeListener implements ActionListener
    {
        private int number;
        private int amount;
        
        public UpdateTimeListener(int number, int amount)
        {
            super();
            this.number = number;
            this.amount = amount;
        }
        
        @Override
        public void actionPerformed(ActionEvent event)
        {
            try
            {
                timerInterfaceAsync.update_time(number, amount);
            }
            catch (Exception e)
            {
            }
        }
    }
    
    public static class TimersObjectListener implements
            ObjectListener<Map<Integer, Map<String, Object>>>
    {
        
        @Override
        public void changed(final Map<Integer, Map<String, Object>> value)
        {
            System.out.println("1");
            SwingUtilities.invokeLater(new Runnable()
            {
                public void run()
                {
                    timersObjectChanged(value);
                }
            });
        }
        
    }
    
    public static class StartupObjectListener implements ObjectListener<Integer>
    {
        
        @Override
        public void changed(final Integer value)
        {
            System.out.println("2");
            SwingUtilities.invokeLater(new Runnable()
            {
                public void run()
                {
                    startupObjectChanged(value);
                }
            });
        }
    }
    
    public static AutobusConnection bus;
    public static TimerInterface timerInterface;
    public static TimerInterface timerInterfaceAsync;
    public static int startupTime = 0;
    public static Timer[] currentTimers = null;
    public static Map<Integer, Timer> currentTimerMap = new HashMap<Integer, Timer>();
    public static Map<Integer, TimerPanel> timerPanelMap =
            new HashMap<Integer, TimerPanel>();
    
    public static JFrame frame;
    public static JTabbedPane tabs;
    public static JPanel addTimerPanel;
    
    // A timer that will be tabbed to on the next change of the timer object
    public static int switchToOnLoad = 0;
    
    public static boolean isShowingDisabled = false;
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        frame = new JFrame("TrayTimer");
        frame.setSize(435, 275);
        frame.setLocationRelativeTo(null);
        JLabel loadingLabel = new JLabel("Loading the Autobus client library...");
        loadingLabel.setHorizontalAlignment(loadingLabel.CENTER);
        frame.getContentPane().add(loadingLabel);
        frame.show();
        revalidate(frame);
        AutobusConnection.init();
        SwingUtilities.invokeLater(new Runnable()
        {
            
            @Override
            public void run()
            {
                disableInterface();
            }
        });
        tabs = new JTabbedPane();
        initAddTimerTab();
        bus = AutobusConnection.create("localhost", AutobusConnection.DEFAULT_PORT);
        bus.add_object_watch("timer", "timers", new TimersObjectListener());
        bus.add_object_watch("timer", "startup", new StartupObjectListener());
        timerInterface = bus.get_interface_proxy("timer", TimerInterface.class);
        timerInterfaceAsync = bus.get_interface_proxy("timer", TimerInterface.class, true);
        System.out.println("Connecting");
        bus.start_connecting();
        System.out.println("Connected");
    }
    
    private static void initAddTimerTab()
    {
        addTimerPanel = new JPanel();
        addTimerPanel.setLayout(new TableLayout(new double[] { TableLayout.FILL },
                new double[] { TableLayout.FILL }));
        JButton addTimerButton = new JButton("Add a new timer");
        addTimerPanel.add(addTimerButton, "0, 0, c, c");
        tabs.addTab("+", addTimerPanel);
    }
    
    public static void timersObjectChanged(Map<Integer, Map<String, Object>> value)
    {
        System.out.println("Timers object is " + value);
        if (value == null)
        {
            currentTimers = null;
            currentTimerMap.clear();
        }
        else
        {
            // I'm getting rather annoyed with generics at the moment, so I'm just casting
            // to an unparameterized Collection.
            currentTimers =
                    AutobusConnection.unpack((Collection) value.values(), Timer.class);
            currentTimerMap.clear();
            for (Timer timer : currentTimers)
                currentTimerMap.put(timer.number, timer);
        }
        reloadTimers();
    }
    
    public static void startupObjectChanged(Integer value)
    {
        System.out.println("Startup object is " + value);
        if (value == null)
            value = 0;
        startupTime = value;
        reloadTimers();
    }
    
    public static void reloadTimers()
    {
        if (startupTime == 0 || currentTimers == null)
        {
            disableInterface();
            return;
        }
        enableInterface();
        Map<Integer, TimerPanel> existingPanels = new HashMap<Integer, TimerPanel>();
        for (int i = 0; i < tabs.getTabCount(); i++)
        {
            Component component = tabs.getComponent(i);
            if (component instanceof TimerPanel)
                existingPanels.put(((TimerPanel) component).number, (TimerPanel) component);
        }
        // This is the list of timers that have appeared on the server since the last time
        // we updated. We need to add tabs for these timers.
        ArrayList<Integer> newTimers = new ArrayList<Integer>();
        // This is the list of timers that have disappeared from the server since the last
        // time we updated. We need to delete this timers' tabs.
        ArrayList<Integer> oldTimers = new ArrayList<Integer>();
        // First, we add all of the local timers to oldTimers.
        for (Integer number : existingPanels.keySet())
            oldTimers.add(number);
        // Now we go through all of the remote timers. We remove each one from oldTimers
        // if it's present in that list, since it exists on the server-side so it's not a
        // timer we need to delete. Then, if it's not in the list of local timers, we add
        // it to newTimers.
        for (Timer timer : currentTimers)
        {
            oldTimers.remove((Object) timer.number);
            if (!existingPanels.containsKey(timer.number))
                newTimers.add(timer.number);
        }
        // Now we remove all of the old timers.
        for (Integer number : oldTimers)
        {
            tabs.remove(existingPanels.get(number));
            timerPanelMap.remove(number);
        }
        // Then we add the new ones.
        for (Integer number : newTimers)
        {
            createTimerTab(number);
        }
        // And last, but not least, we update all of the current timers.
        for (Timer timer : currentTimers)
        {
            updateTimer(timer);
        }
    }
    
    private static void updateTimer(Timer timer)
    {
        String[] time = timer.getPaddedTime();
        String hours = time[0];
        String minutes = time[1];
        String seconds = time[2];
        TimerPanel panel = timerPanelMap.get(timer.number);
        tabs.setTitleAt(tabs.indexOfComponent(panel), "Timer " + timer.number + ": "
            + hours + ":" + minutes + "." + seconds);
        panel.getHours().getValue().setText(hours);
        panel.getMinutes().getValue().setText(minutes);
        panel.getSeconds().getValue().setText(seconds);
        Color selectedColor = new Color(192, 205, 255);
        Color otherColor = null;
        panel.getCountingUpButton().setBackground(
                timer.state == 1 ? selectedColor : otherColor);
        panel.getCountingDownButton().setBackground(
                timer.state == 2 ? selectedColor : otherColor);
        panel.getStoppedButton().setBackground(
                timer.state == 3 ? selectedColor : otherColor);
    }
    
    private static void createTimerTab(Integer number)
    {
        TimerPanel panel = new TimerPanel();
        panel.number = number;
        panel.getTimerLabel().setText("Timer " + number);
        tabs.addTab("Timer " + number + ": Loading", panel);
        timerPanelMap.put(number, panel);
        // Hours
        panel.getHours().getUpButton().addActionListener(
                new UpdateTimeListener(number, 1 * 3600));
        panel.getHours().getDownButton().addActionListener(
                new UpdateTimeListener(number, -1 * 3600));
        panel.getHours().getFastUpButton().addActionListener(
                new UpdateTimeListener(number, 10 * 3600));
        panel.getHours().getFastDownButton().addActionListener(
                new UpdateTimeListener(number, -10 * 3600));
        // Minutes
        panel.getMinutes().getUpButton().addActionListener(
                new UpdateTimeListener(number, 1 * 60));
        panel.getMinutes().getDownButton().addActionListener(
                new UpdateTimeListener(number, -1 * 60));
        panel.getMinutes().getFastUpButton().addActionListener(
                new UpdateTimeListener(number, 10 * 60));
        panel.getMinutes().getFastDownButton().addActionListener(
                new UpdateTimeListener(number, -10 * 60));
        // Seconds
        panel.getSeconds().getUpButton().addActionListener(
                new UpdateTimeListener(number, 1 * 1));
        panel.getSeconds().getDownButton().addActionListener(
                new UpdateTimeListener(number, -1 * 1));
        panel.getSeconds().getFastUpButton().addActionListener(
                new UpdateTimeListener(number, 10 * 1));
        panel.getSeconds().getFastDownButton().addActionListener(
                new UpdateTimeListener(number, -10 * 1));
    }
    
    /**
     * Switches the interface to disabled mode. This removes the tabbed panel and adds a
     * label in its place indicating that TrayTimer can't communicate with the Timer
     * Server. This method is called when libautobus sets the startup time or the timer
     * list to null.
     */
    public static void disableInterface()
    {
        if (isShowingDisabled)
            return;
        isShowingDisabled = true;
        frame.getContentPane().removeAll();
        JLabel loadingLabel =
                new JLabel("TrayTimer can't get the list of timers from timerd.");
        loadingLabel.setHorizontalAlignment(loadingLabel.CENTER);
        frame.getContentPane().add(loadingLabel);
        revalidate(frame);
    }
    
    /**
     * Switches the interface to enabled mode. This adds the tabbed panel to the main
     * frame, then adds/updates timers on it.
     */
    public static void enableInterface()
    {
        if (!isShowingDisabled)
            return;
        isShowingDisabled = false;
        frame.getContentPane().removeAll();
        frame.getContentPane().add(tabs);
        tabs.removeAll();
        tabs.addTab("+", addTimerPanel);
        timerPanelMap.clear();
        revalidate(frame);
    }
    
    public static void revalidate(Component component)
    {
        component.invalidate();
        component.validate();
        component.repaint();
    }
    
    public static void onAnnounce(int number)
    {
    }
    
    public static void onDelete(int number)
    {
        try
        {
            timerInterface.delete(number);
        }
        catch (Exception e)
        {
        }
    }
    
    public static void onStateButton(int number, int newState)
    {
        try
        {
            timerInterfaceAsync.set_attribute(number, "state", newState);
        }
        catch (Exception e)
        {
        }
    }
    
}
