package afn.libautobus_jython;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

import org.python.core.Py;
import org.python.core.PyObject;

public class InterfaceProxyHandler implements InvocationHandler
{
    private AutobusConnection bus;
    private String interfaceName;
    private InterfaceWrapper interfaceWrapper;
    private boolean async;
    
    InterfaceProxyHandler(AutobusConnection bus, String interfaceName, boolean async)
    {
        this.bus = bus;
        this.interfaceName = interfaceName;
        this.interfaceWrapper = bus.get_interface(interfaceName);
        this.async = async;
    }
    
    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable
    {
        if (args == null)
            args = new Object[0];
        try
        {
            FunctionWrapper functionWrapper =
                    interfaceWrapper.get_function(method.getName());
            PyObject result;
            if (async)
                result = functionWrapper.invoke_later_py(args);
            else
                result = functionWrapper.invoke_py(args);
            if (method.getReturnType() == Void.TYPE)
                return null;
            Object javaResult = Py.tojava(result, method.getReturnType());
            if (javaResult == null)
                return AutobusUtils.getDefaultPrimitiveValue(method.getReturnType());
            return javaResult;
        }
        catch (Exception e)
        {
            throw new RuntimeException("Method invocation threw an exception", e);
        }
    }
    
}
