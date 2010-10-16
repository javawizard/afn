package afntest;

import java.util.List;
import java.util.Map;

import afn.libautobus.AutobusConnection;
import afn.libautobus.FunctionWrapper;
import afn.libautobus.InterfaceWrapper;

public class Autobus01
{
    
    /**
     * @param args
     */
    public static void main(String[] args)
    {
        AutobusConnection bus =
                AutobusConnection.create("localhost", AutobusConnection.DEFAULT_PORT);
        bus.connect(0);
        InterfaceWrapper object = bus.get_interface("autobus");
        FunctionWrapper function = object.get_function("list_interfaces");
        List<Map<String, Object>> result = (List<Map<String, Object>>) function.invoke();
        System.out.println("Interfaces:");
        for (Map<String, Object> i : result)
        {
            System.out.println(i);
        }
        bus.shutdown();
    }
}
