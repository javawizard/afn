package afn.traytimer;

public class Timer
{
    public static final int UP = 1;
    public static final int DOWN = 2;
    public static final int STOPPED = 3;
    public int number;
    public int time;
    public int state;
    public String name;
    public boolean announce_on_state_change;
    public int announce_count;
    public int announce_interval;
    
    public int getAbsoluteTime()
    {
        if (state == STOPPED)
            return time;
        if (TrayTimer.startupTime == 0)
            return 0; // We don't know the absolute time at the moment
        if (state == UP)
            return Math.max(0, TrayTimer.startupTime - time);
        if (state == DOWN)
            return Math.max(0, time - TrayTimer.startupTime);
        throw new IllegalStateException();
    }
    
    public int[] getTime()
    {
        int absolute = getAbsoluteTime();
        int seconds = absolute % 60;
        absolute = absolute / 60;
        int minutes = absolute % 60;
        absolute = absolute / 60;
        int hours = absolute;
        return new int[] { hours, minutes, seconds };
    }
    
    public String[] getPaddedTime()
    {
        return padItems(getTime());
    }
    
    private static String[] padItems(int[] values)
    {
        String[] result = new String[values.length];
        for (int i = 0; i < result.length; i++)
            result[i] = pad(values[i]);
        return result;
    }
    
    private static String pad(int value)
    {
        if (("" + value).length() == 1)
            return "0" + value;
        return "" + value;
    }
    
    public String toString()
    {
        return "<afn.traytimer.Timer, state: " + state + ", name: " + name + ", number: "
            + number + ", time: " + time + ">";
    }
}
