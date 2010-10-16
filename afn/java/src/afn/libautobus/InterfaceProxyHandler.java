package afn.libautobus;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

public class InterfaceProxyHandler implements InvocationHandler
{
    private AutobusConnection bus;
    private String interfaceName;
    private InterfaceWrapper interfaceWrapper;
    
    InterfaceProxyHandler(AutobusConnection bus, String interfaceName)
    {
        this.bus = bus;
        this.interfaceName = interfaceName;
        this.interfaceWrapper = bus.get_interface(interfaceName);
    }
    
    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable
    {
        if (args == null)
            args = new Object[0];
        return interfaceWrapper.get_function(method.getName()).invoke(args);
    }
    
}
