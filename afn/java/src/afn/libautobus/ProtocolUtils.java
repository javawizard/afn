package afn.libautobus;

import java.net.Socket;
import java.util.List;

public class ProtocolUtils
{
    /**
     * Closes the specified socket without throwing an exception if it already closed or
     * another problem happens.
     * 
     * @param socket
     */
    public static void close(Socket socket)
    {
        try
        {
            socket.close();
        }
        catch (Exception e)
        {
        }
    }
    
    public static Object invokeStatic(Class type, String method)
    {
        try
        {
            return type.getMethod(method).invoke(null);
        }
        catch (Exception e)
        {
            throw new RuntimeException(e);
        }
    }
    
    public static void setattr(Object object, String name, Object value)
    {
        try
        {
            object.getClass().getField(name).set(object, value);
        }
        catch (Exception e)
        {
            throw new RuntimeException(e);
        }
    }
    
    public static Object getattr(Object object, String name)
    {
        try
        {
            return object.getClass().getField(name).get(object);
        }
        catch (Exception e)
        {
            throw new RuntimeException(e);
        }
    }
    
    public static <T> T constructInstance(Class<T> type)
    {
        try
        {
            return type.getConstructor().newInstance();
        }
        catch (Exception e)
        {
            throw new RuntimeException(e);
        }
    }
}
