package afn.libautobus;

import java.lang.reflect.Proxy;

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
 * create() returns an instance of the libautobus subclass.)
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
     * usually takes ten to fifteen seconds to run.
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
    public <T> T get_interface_proxy(String name, Class<T> interfaceType)
    {
        return (T) Proxy.newProxyInstance(interfaceType.getClassLoader(),
                new Class[] { interfaceType }, new InterfaceProxyHandler(this, name));
    }
    
    public void shutdown()
    {
    }
    
    public void add_object_watch(String interfaceName, String objectName,
            ObjectListener<?> listener)
    {
    }
}
