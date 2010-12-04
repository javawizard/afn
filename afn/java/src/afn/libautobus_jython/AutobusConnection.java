package afn.libautobus_jython;

import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Proxy;
import java.math.BigInteger;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.python.core.Options;
import org.python.core.Py;
import org.python.core.PyDictionary;
import org.python.core.PyInteger;
import org.python.core.PyModule;
import org.python.core.PyObject;
import org.python.core.PyString;
import org.python.util.PythonInterpreter;

/**
 * A connection to an Autobus server. You'll typically connect to an Autobus server with
 * this class and go from there.<br/><br/>
 * 
 * This class uses Jython to run the Python version of libautobus under the covers. You
 * don't need to have Python installed; jython.jar contains everything you need. There are
 * a few quirks, however. Probably the most notable ones are that stack traces may look
 * quite a bit different, the method naming conventions of this class follow Python's
 * conventions instead of Java's, and you create an AutobusConnection by calling
 * {@link #create(String, int)} instead of by using a constructor. (The reason for this is
 * that libautobus.py subclasses AutobusConnection if it's running under Jython, and
 * create() returns an instance of the libautobus subclass. In other words, when running
 * under Python, the python libautobus.AutobusConnection subclasses Python's built-in
 * class object, but when running under Jython, the python libautobus.AutobusConnection
 * subclasses this class and overrides all of its methods that have the same names as one
 * in the python AutobusConnection class.)
 * 
 * @author Alexander Boyd
 * 
 */
public class AutobusConnection
{
    public static final int DEFAULT_PORT = 28862;
    private static PyDictionary pythonNamespace = new PyDictionary();
    private static PythonInterpreter pythonInterpreter =
            new PythonInterpreter(pythonNamespace);
    private static boolean hasLoadedPython = false;
    private static PyModule libautobus;
    
    /**
     * Initializes the Autobus client library. This loads Jython and initializes
     * libautobus. This will be called when the first AutobusConnection is constructed,
     * but it can be called before that to get initialization over and done with. This
     * usually takes ten to fifteen seconds to run due to Jython compiling the Python
     * version of libautobus to Java bytecode.
     */
    public static synchronized void init()
    {
        if (!hasLoadedPython)
        {
            hasLoadedPython = true;
            Options.showJavaExceptions = true;
            Options.includeJavaStackInExceptions = true;
            pythonInterpreter.exec("import sys");
            pythonInterpreter.exec("sys.path += ['../afn-python/src']");
            pythonInterpreter.exec("import libautobus");
            libautobus = (PyModule) pythonNamespace.get("libautobus");
        }
    }
    
    /**
     * <b>This constructor should not be used.</b> Since this class is just a wrapper
     * around the Python version of libautobus running on Jython, most of the methods in
     * this class don't actually do anything. libautobus creates a subclass of this class
     * when it's running under Jython, and it's an instance of this subclass that
     * {@link #create(String, int)} returns. You should therefore use create() instead.
     */
    public AutobusConnection()
    {
    }
    
    public static AutobusConnection create(String host, int port)
    {
        init();
        return Py.tojava(libautobus.invoke("AutobusConnection", new PyString(host),
                new PyInteger(port)), AutobusConnection.class);
    }
    
    public void connect(int attempts)
    {
    }
    
    public InterfaceWrapper get_interface(String name)
    {
        return null;
    }
    
    @SuppressWarnings("unchecked")
    public <T> T get_interface_proxy(String name, Class<T> interfaceType, boolean async)
    {
        return (T) Proxy
                .newProxyInstance(interfaceType.getClassLoader(),
                        new Class[] { interfaceType }, new InterfaceProxyHandler(this,
                                name, async));
    }
    
    public <T> T get_interface_proxy(String name, Class<T> interfaceType)
    {
        return get_interface_proxy(name, interfaceType, false);
    }
    
    public void shutdown()
    {
    }
    
    public void add_object_watch(String interfaceName, String objectName,
            ObjectListener<?> listener)
    {
    }
    
    public void start_connecting()
    {
    }
    
    /**
     * Iterates through all of the items in the specified map. For each item, the field on
     * the specified object whose name is equal to the key of the item has its value set
     * to the item itself, or a default value if the field is primitive and the item's
     * value is null. This is used to follow a common convention among Autobus services of
     * representing structures as maps.<br/><br/>
     * 
     * If a field does not exist for a specified item, that item will be ignored.
     * 
     * @param <T>
     * @param map
     *            The map of items to unpack into the specified object
     * @param object
     *            The object that the items will be unpacked into
     * @return <t>object</t>
     */
    public static <T> T unpack(Map<String, ? extends Object> map, T object)
    {
        //System.out.println("Unpacking " + map + " into " + object);
        for (Entry<String, ?> entry : map.entrySet())
        {
            try
            {
                setattr(object, entry.getKey(), entry.getValue());
            }
            catch (Exception e)
            {
                e.printStackTrace();
            }
        }
        //System.out.println("Object is now " + object);
        return object;
    }
    
    /**
     * For each item in the specified list, creates a new object of the specified type and
     * passes it into {@link #unpack(Map, Object)} with the item in the list. The
     * resulting objects are then returned as an array from this method.
     * 
     * @param <T>
     * @param map
     * @param type
     * @return
     */
    public static <T> T[] unpack(Collection<Map<String, ? extends Object>> list,
            Class<T> type)
    {
        try
        {
            T[] array = (T[]) Array.newInstance(type, list.size());
            Map<String, ? extends Object>[] args =
                    (Map<String, ? extends Object>[]) list.toArray(new Map[0]);
            for (int i = 0; i < array.length; i++)
                array[i] = unpack(args[i], type.getConstructor().newInstance());
            return array;
        }
        catch (Exception e)
        {
            throw new RuntimeException(e);
        }
    }
    
    /**
     * Roughly mirrors Python's setattr function, but for Java objects. It's quite similar
     * to using reflection to set the specified field, but it wraps everything with an
     * unchecked exception (specifically an IllegalArgumentException) and it converts
     * between types wherever possible (I.E. if the supplied value is a java.lang.Long and
     * the field type is int, the long will be converted to an int via a narrowing
     * primitive conversion).
     * 
     * @param object
     *            The object that has a field whose value is going to be set
     * @param name
     *            The name of the field on the object
     * @param value
     *            The new value for the field
     */
    public static void setattr(Object object, String name, Object value)
    {
        try
        {
            Field field = object.getClass().getField(name);
            Class fieldType = field.getType();
            Class valueType = value.getClass();
            if ((fieldType == Integer.TYPE || fieldType == Integer.class)
                && valueType == Long.class)
            {
                long oldValue = (Long) value;
                value = new Integer((int) oldValue);
            }
            else if ((fieldType == Integer.TYPE || fieldType == Integer.class)
                && valueType == BigInteger.class)
            {
                BigInteger oldValue = (BigInteger) value;
                value = oldValue.intValue();
            }
            field.set(object, value);
        }
        catch (Exception e)
        {
            throw new RuntimeException(e);
        }
    }
}
