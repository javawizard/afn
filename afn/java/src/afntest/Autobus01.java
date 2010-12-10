package afntest;

import java.util.List;
import java.util.Map;

import afn.libautobus.AutobusConnection;
import afn.libautobus.InterfaceWrapper;

public class Autobus01
{
    public static interface TimerInterface
    {
        // TODO: finish this once proxies work
        
        // A method that doesn't exist on the actual interface that I was using to
        // test out what would happen when a nonexistent method is called
        public void bogus();
    }
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        AutobusConnection bus = new AutobusConnection();
        bus.connect();
        System.out.println(bus.getInterface("timer").getFunction("list_timers").invoke());
        bus.shutdown();
    }
}
